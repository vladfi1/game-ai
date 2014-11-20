{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Instances where

import NumericPrelude
import qualified Algebra.Ring as Ring
import qualified Algebra.Additive as Additive
import qualified Algebra.Module as Module

import Data.Monoid

--instance (Ring.C a) => Ring.C (b -> a) where
--  (f1 * f2) b = (f1 b) * (f2 b)
--  one = const one
--  fromInteger = const . fromInteger
--  (f ^ n) b = (f b) ^ n

instance (Additive.C a) => Additive.C (Sum a) where
  zero = Sum zero
  (Sum x) + (Sum y) = Sum (x + y)
  (Sum x) - (Sum y) = Sum (x - y)
  negate (Sum x) = Sum (negate x)

instance (Ring.C a) => Ring.C (Sum a) where
  (Sum x) * (Sum y) = Sum (x * y)
  one = Sum one
  fromInteger = Sum . fromInteger
  (Sum x) ^ n = Sum (x ^ n)

instance (Module.C a v) => Module.C a (Sum v) where
  a *> (Sum v) = Sum (a *> v)
