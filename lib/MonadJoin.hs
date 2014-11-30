{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

--{-# LANGUAGE RankNTypes, TypeFamilies #-}

module MonadJoin where

import Prelude hiding (sequence)

import Data.Functor.Compose
import Data.Traversable

import Control.Applicative

class (Applicative m, Monad m) => Monad' m where
  return' :: a -> m a
  bind' :: m a -> (a -> m b) -> m b
  join' :: m (m a) -> m a
  
  --{-# INLINABLE bind' #-}
  bind' ma f = join' (fmap f ma)

instance (Monad' m, Monad' n, Traversable n) => Monad' (Compose m n) where
  --join' :: Compose m n (Compose m n a) -> Compose m n a
  return' = Compose . return' . return'
  --{-# INLINABLE join' #-}
  join' = Compose . (fmap join') . join' . (fmap sequenceA) . getCompose . (fmap getCompose)

-- standard
instance (Monad' m, Monad' n, Traversable n) => Monad (Compose m n) where
  return = return'
  (>>=) = bind'

-- standard
instance Monad' [] where
  return' = return
  --{-# INLINABLE join' #-}
  join' = (>>= id)
  --join' = concat
  

