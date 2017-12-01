{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Control.Monad.World.Replay (
    replay
) where

import Prelude hiding (getLine, putStrLn, readFile, writeFile)

import Control.Monad.World.Arbiter
import Control.Monad.World.Class

import Control.Monad (forM)
import Control.Monad.Trans
import qualified Data.Map as M

data Replay

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

instance Monad m => MonadConcurrent (ArbiterT Replay m) where
    mapConcurrently = mapConcurrentlyImpl

instance Monad m => MonadWorld (ArbiterT Replay m) where
    -- System.Concurrent
    threadDelay = const runOp

    -- System.IO
    getLine = runOp
    putStrLn = const runOp
    readFile = const runOp
    writeFile _ = const runOp

    -- Data.Time
    getCurrentTime = runOp

    -- System.Random
    randomRIO = const runOp

--
-- Implementation of `mapConcurrently`
--

mapConcurrentlyImpl :: (Monad m, Read b, Show b) => (a -> ArbiterT Replay m b) -> [a] -> ArbiterT Replay m [b]
mapConcurrentlyImpl f ax = do
    keyed <- attachKeys ax
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

--
-- Run a replay operation
--

runOp :: (Monad m, Read a, Show a) => ArbiterT Replay m a
runOp = do
    k <- genKey
    runKeyedOp k

runKeyedOp :: (Monad m, Read a, Show a) => Key -> ArbiterT Replay m a
runKeyedOp k = do
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
-- Read a value
--

takeVal :: Monad m => Key -> ArbiterT Replay m (Maybe Value)
takeVal k = do
    let delKey arb = arb { rc = M.delete k (rc arb) }
    val <- M.lookup k . rc <$> get
    modify delKey
    pure val
