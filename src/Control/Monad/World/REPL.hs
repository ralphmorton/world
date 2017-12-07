{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.World.REPL (
    repl
) where

import Prelude hiding (getLine, putStr, putStrLn, readFile, writeFile)

import Control.Monad.World.Class

import Control.Monad.Trans
import Data.Monoid
import qualified System.IO as IO

--
-- Public API
--

-- | Run a computation via REPL
repl :: Monad m => ReplT m a -> m a
repl = runReplT

newtype ReplT m a = ReplT { runReplT :: m a }

instance Functor m => Functor (ReplT m) where
    fmap f (ReplT m) = ReplT (fmap f m)

instance Applicative m => Applicative (ReplT m) where
    pure = ReplT . pure
    (<*>) (ReplT mf) (ReplT ma) = ReplT (mf <*> ma)

instance Monad m => Monad (ReplT m) where
    return = pure
    (>>=) (ReplT ma) f = ReplT $ do
        a <- ma
        runReplT (f a)

instance MonadTrans ReplT where
    lift = ReplT

instance MonadIO m => MonadIO (ReplT m) where
    liftIO = ReplT . liftIO

instance Monad m => MonadConcurrent (ReplT m) where
    type ConcurrentC (ReplT m) b = ConcurrentC m b
    mapConcurrently = mapM

instance (MonadIO m, MonadTime m) => MonadTime (ReplT m) where
    threadDelay = lift . threadDelay
    getCurrentTime = fmap pure $ input "Enter the current time is ISO-8601 format"

instance (MonadIO m, MonadTerminal m) => MonadTerminal (ReplT m) where
    getLine = fmap pure $ input "Enter a line of input"
    putStr = fmap pure . output "putStr"
    putStrLn = fmap pure . output "putStrLn"

instance (MonadIO m, MonadFile m) => MonadFile (ReplT m) where
    readFile = fmap pure . input . ("Enter the contents for file " <>)
    writeFile fp = fmap pure . output ("Contents of file " <> fp)
    readFileT = fmap pure . input . ("Enter the contents for file " <>)
    writeFileT fp = fmap pure . output ("Contents of file " <> fp)
    readFileBS = fmap pure . input . ("Enter the contents for file " <>)
    writeFileBS fp = fmap pure . output ("Contents of file " <> fp)
    readFileLBS = fmap pure . input . ("Enter the contents for file " <>)
    writeFileLBS fp = fmap pure . output ("Contents of file " <> fp)

instance (MonadIO m, MonadRandom m) => MonadRandom (ReplT m) where
    type RandomC (ReplT m) a = (Read a, Show a)
    randomR (s, e) = input $ "Enter a value between " <> show s <> " and " <> show e

input :: (Read a, MonadIO m) => String -> m a
input prompt = liftIO $ do
    IO.putStrLn prompt
    str <- IO.getLine
    case readsPrec 0 str of
        [(a,"")] -> do
            IO.putStrLn "-----\n"
            pure a
        _ -> input prompt

output :: (Show a, MonadIO m) => String -> a -> m ()
output title val = liftIO $ do
    IO.putStrLn (title <> ":")
    IO.putStrLn (show val)
    IO.putStrLn "-----\n"
