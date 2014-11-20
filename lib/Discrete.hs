{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Discrete where

import NumericPrelude hiding (foldr1)
import qualified Algebra.Ring as Ring
import qualified Algebra.Module as Module
import qualified Algebra.Field as Field
import qualified Prelude

import Control.Lens

import Data.Functor.Compose
import Data.Foldable
import Data.Traversable
import Data.Monoid

import MonadJoin

import Control.Monad.Random (Rand)
import qualified Control.Monad.Random as Random
import System.Random (RandomGen)

newtype Weighted w a = Weighted { asPair :: (a, w) }

instance (Show w, Show a) => Show (Weighted w a) where
  show = show . asPair

instance Functor (Weighted w) where
  fmap f = Weighted . (_1 %~ f) . asPair

instance (Ring.C w) => Monad' (Weighted w) where
  return' a = Weighted (a, Ring.one)
  join' (Weighted (Weighted (a, w0), w1)) = Weighted (a, w0 * w1)

-- standard
instance (Ring.C w) => Monad (Weighted w) where
  return = return'
  (>>=) = bind'

instance Foldable (Weighted w) where
  foldMap f (Weighted (a, _)) = f a

instance Traversable (Weighted w) where
  sequenceA (Weighted (as, w)) = fmap (Weighted . (, w)) as

type Discrete w = Compose [] (Weighted w)

fromList :: [(a, w)] -> Discrete w a
fromList = Compose . (map Weighted)

toList :: Discrete w a -> [(a, w)]
toList = (map asPair) . getCompose

(<*) = flip (*>)

expectation :: (Module.C w a) => Discrete w a -> a
expectation = (foldr1 (+)) . fmap ((uncurry (<*)) . asPair) . getCompose

--instance (Show w, Show a) => Show (Discrete w a) where
--  show = show . getCompose

class (Field.C w, Monad m) => MonadDiscrete w m | m -> w where
  sample :: [(a, w)] -> m a
  uniform :: [a] -> m a
  uniform as = sample [(a, w) | a <- as]
    where w = one / fromIntegral (length as)

instance (RandomGen g) => MonadDiscrete Prelude.Rational (Rand g) where
  sample = Random.fromList
  uniform = Random.uniform

instance (Field.C w) => MonadDiscrete w (Discrete w) where
  sample = fromList

--test :: (MonadDiscrete w m) => m a
test = do
  x <- sample [(True, 0.5), (False, 0.5)]
  y <- if x then (return 1) else sample [(1, 0.5), (2, 0.5)]
  return (y + 1)

