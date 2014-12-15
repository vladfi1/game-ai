{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
--{-# LANGUAGE IncoherentInstances #-}

module MonadJoin where

import Data.Functor.Compose
import Data.Traversable
import Data.Foldable
import Data.Monoid
import Control.Applicative

class (Applicative m) => Monad' m where
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

-- necessary because we don't have polymorphic constraints
class Monoid1 m where
  mempty1 :: m a
  mappend1 :: m a -> m a -> m a

newtype WrapMonoid m a = WrapMonoid {unwrapMonoid :: m a}

instance (Monoid1 m) => Monoid (WrapMonoid m a) where
  mempty = WrapMonoid mempty1
  mappend (WrapMonoid ma1) (WrapMonoid ma2) = WrapMonoid (ma1 `mappend1` ma2)

instance (Monoid1 m, Foldable m, Applicative m) => Monad' m where
  return' = pure
  join' = unwrapMonoid . fold . (fmap WrapMonoid)

-- standard
instance Monoid1 [] where
  mempty1 = mempty
  mappend1 = mappend

