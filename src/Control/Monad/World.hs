{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Control.Monad.World (
    MonadWait(..),
    MonadSTM(..),
    MonadWorld(..),
    MonadConcurrent(..),
    AsException(..),
    TerminalException(..),
    FileException(..),
    TimeException(..),
    Tape,
    ArbiterError(..),
    run,
    record,
    replay,
    orThrow
) where

import Prelude hiding (getLine, putStrLn, readFile, writeFile)

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import Control.Exception (Exception, SomeException, handle)
import Control.Lens (Prism', _1, _2, _Just, at, prism', review, over)
import Control.Monad (foldM, forever, forM)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Reader as R
import Control.Monad.State (StateT)
import Control.Monad.Trans
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Time as Time
import qualified System.IO as IO
import qualified System.Random as Random

--
-- Class
--

class Monad m => MonadSTM m where
    atomically :: STM.STM a -> m a

instance MonadSTM IO where
    atomically = STM.atomically

instance MonadSTM m => MonadSTM (ExceptT e m) where
    atomically = lift . atomically

instance MonadSTM m => MonadSTM (ReaderT e m) where
    atomically = lift . atomically

instance MonadSTM m => MonadSTM (StateT e m) where
    atomically = lift . atomically

class Monad m => MonadWait m where
    wait :: Int -> m () -- wait n micros

instance MonadWait IO where
    wait = threadDelay

instance MonadWait m => MonadWait (ExceptT e m) where
    wait = lift . wait

instance MonadWait m => MonadWait (ReaderT e m) where
    wait = lift . wait

instance MonadWait m => MonadWait (StateT e m) where
    wait = lift . wait

class Monad m => MonadConcurrent m where
    mapConcurrently :: (Read b, Show b) => (a -> m b) -> [a] -> m [b] -- TODO: do some stuff with Monoid1 or similar to get traversable

instance MonadConcurrent IO where
    mapConcurrently = Async.mapConcurrently

instance (Read e, Show e, MonadConcurrent m) => MonadConcurrent (ExceptT e m) where
    mapConcurrently f ax = do
        results <- lift $ mapConcurrently (runExceptT . f) ax
        case sequence results of
            Left e -> throwError e
            Right bx -> pure bx

instance MonadConcurrent m => MonadConcurrent (ReaderT r m) where
    mapConcurrently f ax = do
        r <- R.ask
        lift $ mapConcurrently (flip runReaderT r . f) ax

class (MonadSTM m, MonadWait m) => MonadWorld m where
    -- System.IO
    getLine :: m (Either TerminalException String)
    putStrLn :: String -> m (Either TerminalException ())
    readFile :: IO.FilePath -> m (Either FileException String)
    writeFile :: IO.FilePath -> String -> m (Either FileException ())

    -- Data.Time
    getCurrentTime :: m (Either TimeException Time.UTCTime)

    -- System.Random
    randomRIO :: (Read a, Show a, Random.Random a) => (a, a) -> m a

instance MonadWorld IO where
    -- System.IO
    getLine = failSome (TerminalException . show) IO.getLine
    putStrLn = failSome (TerminalException . show) . IO.putStrLn
    readFile = failSome (FileException . show) . IO.readFile
    writeFile fp = failSome (FileException . show) . IO.writeFile fp

    -- Data.Time
    getCurrentTime = failSome (TimeException . show) Time.getCurrentTime

    -- System.Random
    randomRIO = Random.randomRIO

failSome :: (SomeException -> e) -> IO a -> IO (Either e a)
failSome f = handle (pure . Left . f) . fmap Right

instance MonadWorld m => MonadWorld (ExceptT e m) where
    -- System.IO
    getLine = lift getLine
    putStrLn = lift . putStrLn
    readFile = lift . readFile
    writeFile fp = lift . writeFile fp

    -- Data.Time
    getCurrentTime = lift getCurrentTime

    -- System.Random
    randomRIO = lift . randomRIO

instance MonadWorld m => MonadWorld (ReaderT r m) where
    -- System.IO
    getLine = lift getLine
    putStrLn = lift . putStrLn
    readFile = lift . readFile
    writeFile fp = lift . writeFile fp

    -- Data.Time
    getCurrentTime = lift getCurrentTime
    
    -- System.Random
    randomRIO = lift . randomRIO

instance MonadWorld m => MonadWorld (StateT r m) where
    -- System.IO
    getLine = lift getLine
    putStrLn = lift . putStrLn
    readFile = lift . readFile
    writeFile fp = lift . writeFile fp

    -- Data.Time
    getCurrentTime = lift getCurrentTime
    
    -- System.Random
    randomRIO = lift . randomRIO

--
-- Exception types
--

class AsException e e' where
    _Exception :: Prism' e e'

instance AsException e e where
    _Exception = prism' id pure

data TerminalException
    = TerminalException String
    deriving (Eq, Read, Show)

instance Exception TerminalException

data FileException
    = FileException String
    deriving (Eq, Read, Show)

instance Exception FileException

data TimeException
    = TimeException String
    deriving (Eq, Read, Show)

instance Exception TimeException

--
-- Public API
--

-- | Run a computation normally.
run :: (MonadSTM m, MonadWait m) => ArbiterT m a -> m (Either ArbiterError a)
run f = do
    arb <- atomically (mkArbiting Noop M.empty)
    runArbiterT f arb

-- | Record a computation, generating a
-- result, and a tape for replay.
record :: (MonadSTM m, MonadWait m) => ArbiterT m a -> m (Either ArbiterError (Tape, a))
record f = do
    arb <- atomically (mkArbiting Record M.empty)
    res <- runArbiterT f arb
    tape <- (atomically . STM.readTVar) (rc arb)
    pure $ do
        a <- res
        pure (tape, a)

-- | Replay an existing tape.
replay :: (MonadSTM m, MonadWait m) => ArbiterT m a -> Tape -> m (Either ArbiterError a)
replay f tape = do
    arb <- atomically (mkArbiting Replay tape)
    res <- runArbiterT f arb
    depleted <- (atomically . STM.readTVar) (rc arb)
    pure $ do
        a <- res
        case depleted == M.empty of
            True -> pure a
            False -> Left (TapeNotFullyDepleted depleted)

mkArbiting :: ArbiterOp -> Tape -> STM.STM Arbiting
mkArbiting opv tape = do
    kgv <- STM.newTVar 0
    rcv <- STM.newTVar tape
    pure Arbiting {
        op = opv,
        kg = kgv,
        rc = rcv
    }

-- | Given `MonadWorld m, MonadError e m, AsException e e'`,
-- and an `m (Either e' a)`, unwrap the result, throwing an `e`
-- if the result is a Left.
orThrow :: (MonadWorld m, MonadError e m, AsException e e') => m (Either e' a) -> m a
orThrow m = do
    res <- m
    case res of
        Left e -> throwError (review _Exception e)
        Right r -> pure r

--
-- Arbiting
--

data Arbiting = Arbiting {
    op :: ArbiterOp,
    kg :: STM.TVar Key,
    rc :: STM.TVar Tape
}

data ArbiterOp
    = Noop
    | Record
    | Replay

type Tape = M.Map Key Value

type Key = Int

data Value
    = Pending
    | ExistingValue String
    | ExistingTape Tape
    deriving (Eq, Read, Show)

data ArbiterT m a = ArbiterT {
    runArbiterT :: Arbiting -> m (Either ArbiterError a)
}

data ArbiterError
    = ParseFailure String
    | TapeNotFullyDepleted Tape
    | NonExistentKey Key
    | KeyMismatch Key
    | SubtreeError Key ArbiterError
    deriving (Read, Show)

instance Exception ArbiterError

instance Functor m => Functor (ArbiterT m) where
    fmap f (ArbiterT m) = ArbiterT (fmap (fmap f) . m)

instance Monad m => Applicative (ArbiterT m) where
    pure = ArbiterT . const . pure . pure
    (<*>) (ArbiterT mf) (ArbiterT ma) = ArbiterT $ \s -> do
        e1 <- mf s
        case e1 of
            Left err -> pure (Left err)
            Right f -> do
                e2 <- ma s
                case e2 of
                    Left err -> pure (Left err)
                    Right a -> (pure . pure) (f a)

instance Monad m => Monad (ArbiterT m) where
    return = pure
    (>>=) (ArbiterT ma) f = ArbiterT $ \s -> do
        e <- ma s
        case e of
            Left err -> pure (Left err)
            Right a -> runArbiterT (f a) s

instance MonadTrans ArbiterT where
    lift = ArbiterT . const . fmap pure

instance MonadIO m => MonadIO (ArbiterT m) where
    liftIO = lift . liftIO

instance MonadSTM m => MonadSTM (ArbiterT m) where
    atomically = lift . atomically

instance MonadWait m => MonadWait (ArbiterT m) where
    wait = lift . wait

instance (MonadConcurrent m, MonadSTM m, MonadWait m) => MonadConcurrent (ArbiterT m) where
    mapConcurrently = mapConcurrentlyImpl

instance MonadWorld m => MonadWorld (ArbiterT m) where
    -- System.IO
    getLine = runOp getLine
    putStrLn = runOp . putStrLn
    readFile = runOp . readFile
    writeFile fp = runOp . writeFile fp

    -- Data.Time
    getCurrentTime = runOp getCurrentTime

    -- System.Random
    randomRIO = runOp . randomRIO

--
-- Implementation of `mapConcurrently` for `ArbiterT`
--

mapConcurrentlyImpl :: (MonadConcurrent m, MonadSTM m, MonadWait m, Read b, Show b) => (a -> ArbiterT m b) -> [a] -> ArbiterT m [b]
mapConcurrentlyImpl f ax = do
    arb <- ask
    case op arb of
        Noop -> do
            results <- lift $ mapConcurrently (run . f) ax
            case sequence results of
                Left e -> throwArb e
                Right bx -> pure bx
        Record -> do
            keyed <- attachKeys arb ax
            let keys = fst <$> keyed
            atomically $ mapM (flip (writeVal arb) Pending) keys
            results <- lift $ mapConcurrently (record . withKey f) keyed
            case sequence results of
                Left e -> throwArb e
                Right bx -> do
                    atomically . forM bx $ \(t, (k, b)) -> do
                        writeVal arb k (ExistingTape t)
                    pure (snd . snd <$> bx)
        Replay -> do
            keyed <- attachKeys arb ax
            let keys = fst <$> keyed
            fx <- forM keyed $ \(k, a) -> do
                tape <- retrieveKeyTape k
                pure $ replay (f a) tape
            results <- lift $ mapConcurrently id fx
            case sequence results of
                Left e -> throwArb e
                Right bx -> pure bx

attachKeys :: MonadSTM m => Arbiting -> [a] -> m [(Key, a)]
attachKeys arb = atomically . foldM attachKey mempty
    where attachKey rx a = (rx<>) . pure . (,a) <$> genKey arb

withKey :: Monad m => (a -> m b) -> (k, a) -> m (k, b)
withKey f (k, a) = do
    b <- f a
    pure (k, b)

retrieveKeyTape :: (MonadSTM m, MonadWait m) => Key -> ArbiterT m Tape
retrieveKeyTape k = do
    arb <- ask
    mVal <- atomically (takeVal arb k)
    case mVal of
        Just Pending -> do
            forever (wait 1000)
        Just (ExistingValue _) -> do
            throwArb (KeyMismatch k)
        Just (ExistingTape tape) -> do
            pure tape
        Nothing -> do
            throwArb (NonExistentKey k)

--
-- Run an operation, either recording or replaying
--

runOp :: (Read a, Show a, MonadSTM m, MonadWait m) => m a -> ArbiterT m a
runOp f = do
    arb <- ask
    k <- atomically (genKey arb)
    runKeyedOp k (lift f)

runKeyedOp :: (Read a, Show a, MonadSTM m, MonadWait m) => Key -> ArbiterT m a -> ArbiterT m a
runKeyedOp k f = do
    arb <- ask
    case op arb of
        Noop -> f
        Record -> do
            atomically (writeVal arb k Pending)
            a <- f
            atomically (writeVal arb k . ExistingValue $ show a)
            pure a
        Replay -> do
            res <- retrieveKeyValue arb k
            case res of
                Left e -> throwArb e
                Right Nothing -> forever (wait 1000)
                Right (Just a) -> pure a

retrieveKeyValue :: (Read a, MonadSTM m, MonadWait m) => Arbiting -> Key -> m (Either ArbiterError (Maybe a))
retrieveKeyValue arb k = do
    mVal <- atomically (takeVal arb k)
    case mVal of
        Just Pending -> do
            pure (Right Nothing)
        Just (ExistingValue str) -> do
            case readsPrec 0 str of
                [(a, "")] -> (pure . Right) (Just a)
                _ -> (pure . Left) (ParseFailure str)
        Just (ExistingTape _) -> do
            (pure . Left) (KeyMismatch k)
        Nothing -> do
            (pure . Left) (NonExistentKey k)

--
-- Basic manipulation
--

ask :: Applicative m => ArbiterT m Arbiting
ask = ArbiterT (pure . pure)

--
-- Error
--

throwArb :: Monad m => ArbiterError -> ArbiterT m a
throwArb err = (ArbiterT . const) (pure $ Left err)

--
-- Generate a key
--

genKey :: Arbiting -> STM.STM Key
genKey arb = do
    let kgv = kg arb
    v <- succ <$> STM.readTVar kgv
    STM.writeTVar kgv v
    pure v

--
-- Read a value
--

takeVal :: Arbiting -> Key -> STM.STM (Maybe Value)
takeVal arb k = do
    let rcv = rc arb
    mVal <- M.lookup k <$> STM.readTVar rcv
    STM.modifyTVar rcv (M.delete k)
    pure mVal

--
-- Write a value
--

writeVal :: Arbiting -> Key -> Value -> STM.STM ()
writeVal arb k v = do
    let rcv = rc arb
    STM.modifyTVar rcv (M.insert k v)
