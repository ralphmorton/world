{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Control.Monad.World.Record (
    record
) where

import Prelude hiding (getLine, putStrLn, readFile, writeFile)

import Control.Monad.World.Arbiter
import Control.Monad.World.Class

import Control.Lens (_1, over)
import Control.Monad (forM_)
import Control.Monad.Trans
import qualified Data.Map as M

data Record

--
-- Public API
--

-- | Record a computation, generating a
-- result, and a tape for replay.
record :: Monad m => ArbiterT Record m a -> m (Either ArbiterError (Tape, a))
record f = do
    let arb = Arbiting 0 M.empty
    fmap (over _1 rc) <$> runArbiterT f arb

--
-- Instances
--

instance MonadConcurrent m => MonadConcurrent (ArbiterT Record m) where
    mapConcurrently = mapConcurrentlyImpl

instance MonadWorld m => MonadWorld (ArbiterT Record m) where
    -- System.Concurrent
    threadDelay = runOp . threadDelay

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
-- Implementation of `mapConcurrently`
--

mapConcurrentlyImpl :: (MonadConcurrent m, Read b, Show b) => (a -> ArbiterT Record m b) -> [a] -> ArbiterT Record m [b]
mapConcurrentlyImpl f ax = do
    keyed <- attachKeys ax
    results <- lift $ mapConcurrently (record . withKey f) keyed
    case sequence results of
        Left e -> throwArb e
        Right bx -> do
            forM_ bx $ \(t, (k, _)) -> do
                writeVal k (VTape t)
            pure (snd . snd <$> bx)

withKey :: Monad m => (a -> m b) -> (k, a) -> m (k, b)
withKey f (k, a) = do
    b <- f a
    pure (k, b)

--
-- Run a record operation
--

runOp :: (Monad m, Read a, Show a) => m a -> ArbiterT Record m a
runOp f = do
    k <- genKey
    runKeyedOp k (lift f)

runKeyedOp :: (Monad m, Read a, Show a) => Key -> ArbiterT Record m a -> ArbiterT Record m a
runKeyedOp k f = do
    a <- f
    writeVal k (VData $ show a)
    pure a

--
-- Write a value
--

writeVal :: Monad m => Key -> Value -> ArbiterT Record m ()
writeVal k v = modify insKey
    where insKey arb = arb { rc = M.insert k v (rc arb) }
