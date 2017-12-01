{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Control.Monad.World.Arbiter (
    Key,
    Tape,
    Value(..),
    Arbiting(..),
    ArbiterError(..),
    ArbiterT(..),
    get,
    put,
    modify,
    throwArb,
    genKey,
    attachKeys
) where

import Prelude hiding (getLine, putStrLn, readFile, writeFile)

import Control.Arrow ((&&&))
import Control.Exception (Exception)
import Control.Lens (_2, over)
import Control.Monad (foldM)
import Control.Monad.Trans
import qualified Data.Map as M
import Data.Monoid ((<>))

type Key = Int

type Tape = M.Map Key Value

data Value
    = VData String
    | VTape Tape
    deriving (Eq, Read, Show)

data Arbiting = Arbiting {
    kg :: Key,
    rc :: Tape
}

data ArbiterError
    = ParseFailure String
    | TapeNotFullyDepleted Tape
    | NonExistentKey Key
    | KeyMismatch Key
    | SubtreeError Key ArbiterError
    deriving (Read, Show)

instance Exception ArbiterError

data ArbiterT c m a = ArbiterT {
    runArbiterT :: Arbiting -> m (Either ArbiterError (Arbiting, a))
}

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
-- Attach keys
--

attachKeys :: Monad m => [a] -> ArbiterT c m [(Key, a)]
attachKeys = foldM attachKey mempty
    where attachKey rx a = (rx<>) . pure . (,a) <$> genKey
