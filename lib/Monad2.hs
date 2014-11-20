{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

--{-# LANGUAGE RankNTypes, TypeFamilies #-}

module Monad2 where

import Prelude hiding (sequence)

import Data.Functor.Compose
import Data.Traversable

class (Functor m) => Monad' m where
  return' :: a -> m a
  bind' :: m a -> (a -> m b) -> m b
  join' :: m (m a) -> m a
  
  bind' ma f = join' (fmap f ma)

instance (Monad m, Monad' m, Monad n, Monad' n, Traversable n) => Monad' (Compose m n) where
  --join' :: Compose m n (Compose m n a) -> Compose m n a
  return' = Compose . return' . return'
  join' = Compose . (fmap join') . join' . (fmap sequence) . getCompose . (fmap getCompose)

-- standard
instance (Monad m, Monad' m, Monad n, Monad' n, Traversable n) => Monad (Compose m n) where
  return = return'
  ma >>= f = join' (fmap f ma)

-- standard
instance Monad' [] where
  return' = return
  join' = (>>= id) -- concat

