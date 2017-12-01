{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Control.Monad.World.Exception (
    AsException(..),
    orThrow
) where

import Control.Monad.World.Class (MonadWorld)

import Control.Lens (Prism', prism', review)
import Control.Monad.Except (MonadError, throwError)

class AsException e e' where
    _Exception :: Prism' e e'

instance AsException e e where
    _Exception = prism' id pure

-- | Given `MonadWorld m, MonadError e m, AsException e e'`,
-- and an `m (Either e' a)`, unwrap the result, throwing an `e`
-- if the result is a Left.
orThrow :: (MonadWorld m, MonadError e m, AsException e e') => m (Either e' a) -> m a
orThrow m = do
    res <- m
    case res of
        Left e -> throwError (review _Exception e)
        Right r -> pure r
