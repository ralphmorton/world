{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Control.Monad.World (
    MonadWorld(..),
    MonadConcurrent(..),
    AsException(..),
    TerminalException(..),
    FileException(..),
    TimeException(..),
    Tape,
    ArbiterError(..),
    record,
    replay,
    orThrow
) where

import Prelude hiding (getLine, putStrLn, readFile, writeFile)

import Control.Arrow ((&&&))
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
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

class Monad m => MonadWorld m where
    -- System.Concurrent
    threadDelay :: Int -> m ()

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
    -- System.Concurrent
    threadDelay = Concurrent.threadDelay

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
    -- System.Concurrent
    threadDelay = lift . threadDelay

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
    -- System.Concurrent
    threadDelay = lift . threadDelay

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
    -- System.Concurrent
    threadDelay = lift . threadDelay

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

-- | Record a computation, generating a
-- result, and a tape for replay.
record :: Monad m => ArbiterT Record m a -> m (Either ArbiterError (Tape, a))
record f = do
    let arb = Arbiting 0 M.empty
    fmap (over _1 rc) <$> runArbiterT f arb

-- | Replay an existing tape.
replay :: Monad m => ArbiterT Replay m a -> Tape -> m (Either ArbiterError a)
replay f tape = do
    let arb = Arbiting 0 tape
    res <- runArbiterT f arb
    pure $ do
        (arb', a) <- res
        let depleted = rc arb'
        case depleted == M.empty of
            True -> pure a
            False -> Left (TapeNotFullyDepleted depleted)

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
    kg :: Key,
    rc :: Tape
}

type Tape = M.Map Key Value

type Key = Int

data Value
    = VData String
    | VTape Tape
    deriving (Eq, Read, Show)

data Record
data Replay

data ArbiterT c m a = ArbiterT {
    runArbiterT :: Arbiting -> m (Either ArbiterError (Arbiting, a))
}

data ArbiterError
    = ParseFailure String
    | TapeNotFullyDepleted Tape
    | NonExistentKey Key
    | KeyMismatch Key
    | SubtreeError Key ArbiterError
    deriving (Read, Show)

instance Exception ArbiterError

instance Functor m => Functor (ArbiterT c m) where
    fmap f (ArbiterT m) = ArbiterT (fmap (fmap $ over _2 f) . m)

instance Monad m => Applicative (ArbiterT c m) where
    pure a = ArbiterT $ \s -> (pure . pure) (s,a)
    (<*>) (ArbiterT mf) (ArbiterT ma) = ArbiterT $ \s -> do
        e1 <- mf s
        case e1 of
            Left err -> pure (Left err)
            Right (s', f) -> do
                e2 <- ma s'
                case e2 of
                    Left err -> pure (Left err)
                    Right (s'', a) -> (pure . pure) (s'', f a)

instance Monad m => Monad (ArbiterT c m) where
    return = pure
    (>>=) (ArbiterT ma) f = ArbiterT $ \s -> do
        e <- ma s
        case e of
            Left err -> pure (Left err)
            Right (s', a) -> runArbiterT (f a) s'

instance MonadTrans (ArbiterT c) where
    lift ma = ArbiterT $ \s -> fmap (pure . (s,)) ma

instance MonadIO m => MonadIO (ArbiterT c m) where
    liftIO = lift . liftIO

instance MonadConcurrent m => MonadConcurrent (ArbiterT Record m) where
    mapConcurrently = mapConcurrentlyRecordImpl

instance Monad m => MonadConcurrent (ArbiterT Replay m) where
    mapConcurrently = mapConcurrentlyReplayImpl

instance MonadWorld m => MonadWorld (ArbiterT Record m) where
    -- System.Concurrent
    threadDelay = runRecordOp . threadDelay

    -- System.IO
    getLine = runRecordOp getLine
    putStrLn = runRecordOp . putStrLn
    readFile = runRecordOp . readFile
    writeFile fp = runRecordOp . writeFile fp

    -- Data.Time
    getCurrentTime = runRecordOp getCurrentTime

    -- System.Random
    randomRIO = runRecordOp . randomRIO

instance Monad m => MonadWorld (ArbiterT Replay m) where
    -- System.Concurrent
    threadDelay = const runReplayOp

    -- System.IO
    getLine = runReplayOp
    putStrLn = const runReplayOp
    readFile = const runReplayOp
    writeFile _ = const runReplayOp

    -- Data.Time
    getCurrentTime = runReplayOp

    -- System.Random
    randomRIO = const runReplayOp

--
-- Implementation of `mapConcurrently` for `ArbiterT Record` and `ArbiterT Replay`
--

-- Record

mapConcurrentlyRecordImpl :: (MonadConcurrent m, Read b, Show b) => (a -> ArbiterT Record m b) -> [a] -> ArbiterT Record m [b]
mapConcurrentlyRecordImpl f ax = do
    keyed <- attachKeys ax
    let keys = fst <$> keyed
    results <- lift $ mapConcurrently (record . withKey f) keyed
    case sequence results of
        Left e -> throwArb e
        Right bx -> do
            forM bx $ \(t, (k, b)) -> do
                writeVal k (VTape t)
            pure (snd . snd <$> bx)

withKey :: Monad m => (a -> m b) -> (k, a) -> m (k, b)
withKey f (k, a) = do
    b <- f a
    pure (k, b)

-- Replay

mapConcurrentlyReplayImpl :: (Monad m, Read b, Show b) => (a -> ArbiterT Replay m b) -> [a] -> ArbiterT Replay m [b]
mapConcurrentlyReplayImpl f ax = do
    keyed <- attachKeys ax
    let keys = fst <$> keyed
    fx <- forM keyed $ \(k, a) -> do
        tape <- retrieveKeyTape k
        pure $ replay (f a) tape
    results <- lift $ mapM id fx
    case sequence results of
        Left e -> throwArb e
        Right bx -> pure bx

retrieveKeyTape :: Monad m => Key -> ArbiterT Replay m Tape
retrieveKeyTape k = do
    mVal <- takeVal k
    case mVal of
        Just (VData _) -> do
            throwArb (KeyMismatch k)
        Just (VTape tape) -> do
            pure tape
        Nothing -> do
            throwArb (NonExistentKey k)

-- Util

attachKeys :: Monad m => [a] -> ArbiterT c m [(Key, a)]
attachKeys = foldM attachKey mempty
    where attachKey rx a = (rx<>) . pure . (,a) <$> genKey

--
-- Run a record operation
--

runRecordOp :: (Monad m, Read a, Show a) => m a -> ArbiterT Record m a
runRecordOp f = do
    k <- genKey
    runKeyedRecordOp k (lift f)

runKeyedRecordOp :: (Monad m, Read a, Show a) => Key -> ArbiterT Record m a -> ArbiterT Record m a
runKeyedRecordOp k f = do
    a <- f
    writeVal k (VData $ show a)
    pure a

--
-- Run a replay operation
--

runReplayOp :: (Monad m, Read a, Show a) => ArbiterT Replay m a
runReplayOp = do
    k <- genKey
    runKeyedReplayOp k

runKeyedReplayOp :: (Monad m, Read a, Show a) => Key -> ArbiterT Replay m a
runKeyedReplayOp k = do
    res <- retrieveKeyValue k
    case res of
        Left e -> throwArb e
        Right a -> pure a

retrieveKeyValue :: (Monad m, Read a) => Key -> ArbiterT Replay m (Either ArbiterError a)
retrieveKeyValue k = do
    mVal <- takeVal k
    case mVal of
        Just (VData str) -> do
            case readsPrec 0 str of
                [(a, "")] -> pure (Right a)
                _ -> (pure . Left) (ParseFailure str)
        Just (VTape _) -> do
            (pure . Left) (KeyMismatch k)
        Nothing -> do
            (pure . Left) (NonExistentKey k)

--
-- Basic manipulation
--

get :: Applicative m => ArbiterT c m Arbiting
get = ArbiterT $ (pure . pure) . (id &&& id)

put :: Applicative m => Arbiting -> ArbiterT c m ()
put = ArbiterT . const . pure . pure . (,())

modify :: Monad m => (Arbiting -> Arbiting) -> ArbiterT c m ()
modify f = put . f =<< get

--
-- Error
--

throwArb :: Monad m => ArbiterError -> ArbiterT c m a
throwArb err = (ArbiterT . const) (pure $ Left err)

--
-- Generate a key
--

genKey :: Monad m => ArbiterT c m Key
genKey = do
    let incKey arb = arb { kg = succ (kg arb) }
    modify incKey
    kg <$> get

--
-- Read a value
--

takeVal :: Monad m => Key -> ArbiterT Replay m (Maybe Value)
takeVal k = do
    let delKey arb = arb { rc = M.delete k (rc arb) }
    val <- M.lookup k . rc <$> get
    modify delKey
    pure val

--
-- Write a value
--

writeVal :: Monad m => Key -> Value -> ArbiterT Record m ()
writeVal k v = modify insKey
    where insKey arb = arb { rc = M.insert k v (rc arb) }
