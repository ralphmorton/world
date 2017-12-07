{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.World.Record (
    record
) where

import Prelude hiding (getLine, putStr, putStrLn, readFile, writeFile)

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
    type ConcurrentC (ArbiterT Record m) b = (Read b, Show b, ConcurrentC m (Either ArbiterError (Tape, (Key, b))))
    mapConcurrently = mapConcurrentlyImpl

instance MonadTime m => MonadTime (ArbiterT Record m) where
    threadDelay = runOp . threadDelay
    getCurrentTime = runOp getCurrentTime

instance MonadTerminal m => MonadTerminal (ArbiterT Record m) where
    getLine = runOp getLine
    putStr = runOp . putStr
    putStrLn = runOp . putStrLn

instance MonadFile m => MonadFile (ArbiterT Record m) where
    readFile = runOp . readFile
    writeFile fp = runOp . writeFile fp
    readFileT = runOp . readFileT
    writeFileT fp = runOp . writeFileT fp
    readFileBS = runOp . readFileBS
    writeFileBS fp = runOp . writeFileBS fp
    readFileLBS = runOp . readFileLBS
    writeFileLBS fp = runOp . writeFileLBS fp

instance MonadRandom m => MonadRandom (ArbiterT Record m) where
    randomR = runOp . randomR

--
-- Implementation of `mapConcurrently`
--

mapConcurrentlyImpl :: (Read b, Show b, MonadConcurrent m, ConcurrentC m (Either ArbiterError (Tape, (Key, b)))) => (a -> ArbiterT Record m b) -> [a] -> ArbiterT Record m [b]
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
