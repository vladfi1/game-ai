--{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies, TypeFamilies #-}
--{-# LANGUAGE NoMonomorphismRestriction #-}

module Discrete where

import Control.Lens

import Control.Monad.Random (Rand, MonadRandom, fromList)
import System.Random (RandomGen)

newtype Weighted w a = Weighted { asPair :: (a, w) } deriving (Show)
newtype Discrete w a = Discrete { asList :: [Weighted w a] } deriving (Show)

toWeightedList = (map asPair) . asList
fromWeightedList = Discrete . (map Weighted)

modA f = Weighted . (_1 %~ f) . asPair
modW f = Weighted . (_2 %~ f) . asPair

instance Functor (Weighted w) where
  fmap = modA

-- Monad defined in terms of fmap and join
class (Functor m) => Monad' m where
  return' :: a -> m a
  join' :: m (m a) -> m a

-- Monad' is a Monad
-- but this causes undecidable instances :(
--instance (Monad' m) => Monad m where
--  return = return'
--  ma >>= f = join' $ fmap f ma

instance (Num w) => Monad' (Weighted w) where
  return' a = Weighted (a, 1)
  join' (Weighted (Weighted (a, w1), w0)) = Weighted (a, w1 * w0)

-- this is the same for all monads
instance (Num w) => Monad (Weighted w) where
  return = return'
  ma >>= f = join' $ fmap f ma

instance Functor (Discrete w) where
  fmap f = Discrete . (fmap (fmap f)) . asList

reWeight :: (Num w) => (Weighted w (Discrete w a)) -> Discrete w a
reWeight (Weighted (Discrete aws, w0)) = Discrete $ map (modW (* w0)) aws

instance (Num w) => Monad' (Discrete w) where
  return' a = Discrete [return a]
  join' (Discrete aws) = Discrete . concat $ map (asList . reWeight) aws

-- this is the same for all monads
instance (Num w) => Monad (Discrete w) where
  return = return'
  ma >>= f = join' $ fmap f ma

class (Num w, Monad m) => MonadDiscrete w m | m -> w where
  sample :: [(a, w)] -> m a
  --sample :: Discrete w a -> m a

--class (Monad m) => MonadDiscrete m where
  --data Weight m :: *
  --type Weight m
  --sample :: [(a, Weight m)] -> m a


--instance (MonadRandom m) => MonadDiscrete m where
--instance (MonadRandom m) => MonadDiscrete Rational m where
  --type Weight m = Rational
  --sample = fromList . toWeightedList
  --sample = fromList

instance RandomGen g => MonadDiscrete Rational (Rand g) where
  sample = fromList

instance (Num w) => MonadDiscrete w (Discrete w) where
--instance (Fractional w) => MonadDiscrete w (Discrete w) where
--instance MonadDiscrete Double (Discrete Double) where
  --type Weight (Discrete w) = w
  --data Weight (Discrete w) = Weight w
  sample = fromWeightedList
  --sample = id

test1 :: (Monad m) => m String
test1 = return "x"

test2 :: (Num w, MonadDiscrete w m) => m String
test2 = sample [("x", 1)]

test3 :: Discrete Double Int
test3 = do
  x <- sample [(True, 0.5), (False, 0.5)]
  y <- if x then (return 1) else sample [(1, 0.5), (2, 0.5)]
  return (y + 1)

