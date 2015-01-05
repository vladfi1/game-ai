{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
--{-# LANGUAGE OverlappingInstances #-}

module Discrete where

import NumericPrelude hiding (foldl')
import qualified Algebra.Ring as Ring
import qualified Algebra.Module as Module
import qualified Algebra.Field as Field
import qualified Prelude

import Debug.Trace

--import Control.Lens

import Data.Functor.Compose
import Data.Foldable
import Data.Traversable
import Data.Monoid

import MonadJoin

import Control.Monad.Random (Rand)
import qualified Control.Monad.Random as Random
import System.Random (RandomGen)
import Control.Monad (liftM, ap)
import Control.Applicative (Applicative, pure, (<*>))
import Control.Monad.IO.Class

import Data.Set (Set)
import qualified Data.Set as Set

newtype Weighted w a = Weighted { asPair :: (a, w) }

instance (Show w, Show a) => Show (Weighted w a) where
  show = show . asPair

instance Functor (Weighted w) where
  --{-# INLINABLE fmap #-}
  --fmap f = Weighted . (_1 %~ f) . asPair
  fmap f (Weighted (a, w)) = Weighted (f a, w)

instance (Ring.C w) => Monad' (Weighted w) where
  return' a = Weighted (a, one)
  --{-# INLINABLE join' #-}
  join' (Weighted (Weighted (a, w0), w1)) = Weighted (a, w0 * w1)

-- standard
instance (Ring.C w) => Monad (Weighted w) where
  return = return'
  (>>=) = bind'

--standard
instance (Ring.C w) => Applicative (Weighted w) where
  pure = return
  (<*>) = ap

instance Foldable (Weighted w) where
  foldMap f (Weighted (a, _)) = f a

instance Traversable (Weighted w) where
  --{-# INLINABLE sequenceA #-}
  sequenceA (Weighted (as, w)) = fmap (Weighted . (, w)) as

type Discrete w = Compose [] (Weighted w)

fromList :: [(a, w)] -> Discrete w a
fromList = Compose . (map Weighted)

toWeightedList :: Discrete w a -> [(a, w)]
toWeightedList = (map asPair) . getCompose

(<*) = flip (*>)

--{-# INLINABLE expectation #-}
expectation :: (Module.C w a) => Discrete w a -> a
expectation = (foldl' (+) zero) . fmap ((uncurry (<*)) . asPair) . getCompose

--expectation' :: (Module.C w a) => Int -> Discrete w a -> a
--expectation' n = expectation . fromList . (take n) . toWeightedList

{-
instance (Ring.C w) => Monad (Discrete w) where
  return = return'
  
  (Compose []) >>= f = Compose []
  (Compose ((Weighted (a, w)) : rest)) >>= f =
    Compose $ map g (getCompose (f a)) ++ getCompose (Compose rest >>= f)
    where g (Weighted (b, w1)) = Weighted (b, w * w1)
-}

--instance (Show w, Show a) => Show (Discrete w a) where
--  show = show . getCompose

class (Field.C w, Applicative m, Monad m) => MonadDiscrete w m | m -> w where
  sample :: [(a, w)] -> m a
  uniform :: [a] -> m a
  uniform as = sample [(a, w) | a <- as]
    where w = one / fromIntegral (length as)

instance (RandomGen g) => MonadDiscrete Prelude.Rational (Rand g) where
  sample = Random.fromList
  uniform = Random.uniform

instance MonadDiscrete Prelude.Rational IO where
  sample = Random.fromList
  uniform = Random.uniform

dropZeros = filter $ (/= zero) . snd

instance (Eq w, Field.C w) => MonadDiscrete w (Discrete w) where
  sample = fromList . dropZeros

categorical = sample

bernoulli :: (MonadDiscrete w m) => w -> m Bool
bernoulli p = sample [(False, one-p), (True, p)]

choose :: (Field.C w, MonadDiscrete w m) => Int -> [a] -> m [a]
choose k list = go k (length list) list
  where go 0 _ _ = return []
        go _ 0 _ = error "Can't choose from empty list."
        go k n (a:as) = do
          pick <- bernoulli $ (fromIntegral k) / (fromIntegral n)
          if pick
            then liftM (a :) (go (k-1) (n-1) as)
            else go k (n-1) as

--test :: (MonadDiscrete w m) => m a
test = do
  x <- sample [(True, 0.5), (False, 0.5)]
  y <- if x then (return 1) else sample [(1, 0.5), (2, 0.5)]
  return (y + 1)

