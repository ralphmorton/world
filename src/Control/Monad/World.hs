{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Control.Monad.World (
    MonadWorld(..),
    RecordingError(..),
    record,
    replay
) where

import Prelude hiding (getLine, putStrLn, readFile, writeFile)

import Control.Lens (_1, _2, _Just, at, over)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans
import qualified Data.Map as M
import Data.Monoid ((<>))
import qualified Data.Time as Time
import qualified System.IO as IO

class Monad m => MonadWorld m where
    -- System.IO
    getLine :: m String
    putStrLn :: String -> m ()
    readFile :: IO.FilePath -> m String
    writeFile :: IO.FilePath -> String -> m ()

    -- Data.Time
    getCurrentTime :: m Time.UTCTime

instance MonadWorld IO where
    -- System.IO
    getLine = IO.getLine
    putStrLn = IO.putStrLn
    readFile = IO.readFile
    writeFile = IO.writeFile

    -- Data.Time
    getCurrentTime = Time.getCurrentTime

instance MonadWorld m => MonadWorld (ExceptT e m) where
    -- System.IO
    getLine = lift getLine
    putStrLn = lift . putStrLn
    readFile = lift . readFile
    writeFile fp = lift . writeFile fp

    -- Data.Time
    getCurrentTime = lift getCurrentTime

instance MonadWorld m => MonadWorld (ReaderT r m) where
    -- System.IO
    getLine = lift getLine
    putStrLn = lift . putStrLn
    readFile = lift . readFile
    writeFile fp = lift . writeFile fp

    -- Data.Time
    getCurrentTime = lift getCurrentTime

instance MonadWorld m => MonadWorld (StateT r m) where
    -- System.IO
    getLine = lift getLine
    putStrLn = lift . putStrLn
    readFile = lift . readFile
    writeFile fp = lift . writeFile fp

    -- Data.Time
    getCurrentTime = lift getCurrentTime

--
-- Public API
--

record :: Monad m => RecordT m a -> m (Either RecordingError (M.Map String [String], a))
record f = fmap (over _1 rc) <$> runRecordT f (Recording Record M.empty)

replay :: Monad m => RecordT m a -> M.Map String [String] -> m (Either RecordingError a)
replay f tape = do
    res <- runRecordT f (Recording Replay tape)
    pure $ do
        (Recording _ depleted, a) <- res
        case isCompletedDepleted depleted of
            True -> pure a
            False -> Left TapeNotFullyDepleted

isCompletedDepleted :: M.Map String [String] -> Bool
isCompletedDepleted = all id . fmap null . fmap snd . M.toList

--
-- Recording
--

data Recording = Recording {
    op :: RecordOp,
    rc :: M.Map String [String]
} deriving (Eq, Show)

data RecordOp
    = Record
    | Replay
    deriving (Eq, Show)

data RecordT m a
    = RecordT { runRecordT :: Recording -> m (Either RecordingError (Recording, a)) }

data RecordingError
    = TapeNotFullyDepleted
    | TapeTrackDepleted
    | TapeTrackUnparseable String
    deriving (Eq, Show)

instance Functor m => Functor (RecordT m) where
    fmap f (RecordT m) = RecordT (fmap (fmap $ over _2 f) . m)

instance Monad m => Applicative (RecordT m) where
    pure v = RecordT $ \s -> (pure . pure) (s, v)
    (<*>) (RecordT mf) (RecordT ma) = RecordT $ \s -> do
        e1 <- mf s
        case e1 of
            Left err -> pure (Left err)
            Right (s', f) -> do
                e2 <- ma s'
                case e2 of
                    Left err -> pure (Left err)
                    Right (s'', a) -> (pure . pure) (s'', f a)

instance Monad m => Monad (RecordT m) where
    return = pure
    (>>=) (RecordT ma) f = RecordT $ \s -> do
        e <- ma s
        case e of
            Left err -> pure (Left err)
            Right (s', a) -> runRecordT (f a) s'

instance MonadTrans RecordT where
    lift ma = RecordT (fmap pure . (flip (,) <$> ma <*>) . pure)

instance MonadWorld m => MonadWorld (RecordT m) where
    getLine = do
        let key = mkKey "getLine" []
        runOp getLine key
    putStrLn s = do
        let key = mkKey "putStrLn" [s]
        runOp (putStrLn s) key
    readFile fp = do
        let key = mkKey "readFile" [fp]
        runOp (readFile fp) key
    writeFile fp v = do
        let key = mkKey "writeFile" [fp, v]
        runOp (writeFile fp v) key
    getCurrentTime = do
        let key = mkKey "getCurrentTime" []
        runOp getCurrentTime key

--
-- Basic manipulation
--

get :: Applicative m => RecordT m Recording
get = RecordT $ \s -> (pure . pure) (s, s)

put :: Applicative m => Recording -> RecordT m ()
put s = RecordT . const $ (pure . pure) (s, ())

modify :: Monad m => (Recording -> Recording) -> RecordT m ()
modify f = put . f =<< get

--
-- Error
--

apocalypse :: Monad m => RecordingError -> RecordT m a
apocalypse err = (RecordT . const) (pure $ Left err)

--
-- Make a key
--

mkKey :: String -> [String] -> String
mkKey op = foldr (<>) "" . (op:)

--
-- Run an operation, either recording or replaying
--

runOp :: (Read a, Show a, Monad m) => m a -> String -> RecordT m a
runOp f key = do
    Recording op _ <- get
    case op of
        Record -> do
            a <- lift f
            recordOp key a
            pure a
        Replay -> do
            replayOp key

--
-- Record an operation
--

recordOp :: (Show a, Monad m) => String -> a -> RecordT m ()
recordOp key val = modify f
    where
    mf = M.insertWith (<>) key [show val]
    f (Recording op m) = Recording op (mf m)

--
-- Replay an operation
--

replayOp :: (Read a, Monad m) => String -> RecordT m a
replayOp key = do
    str <- popKey key
    case readsPrec 0 str of
        [(a, "")] -> pure a
        _ -> apocalypse (TapeTrackUnparseable str)

popKey :: Monad m => String -> RecordT m String
popKey key = do
    Recording op m <- get
    let vals = M.lookup key m
    let m' = over (at key . _Just) tail m
    put (Recording op m')
    case vals of
        Just (v:_) -> pure v
        _ -> apocalypse TapeTrackDepleted
