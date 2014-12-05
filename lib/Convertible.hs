{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}

module Convertible where

import Data.Foldable

import Data.Packed.Vector (Vector)
import qualified Data.Packed.Vector as Vector
import Foreign.Storable

import Data.BitVector (BV)
import qualified Data.BitVector as BV

class Convertible a b where
  convert :: a -> b

instance (Monad m) => Convertible a (m a) where
  convert = return

instance (Functor f, Convertible a b) => Convertible (f a) (f b) where
  convert = fmap convert

instance (Integral a, Num b) => Convertible a b where
  convert = fromIntegral

instance (Foldable f, Convertible a b, Storable b) => Convertible (f a) (Vector b) where
  convert = Vector.fromList . convert . toList

instance (Num a) => Convertible Bool a where
  convert False = 0
  convert True = 1

instance (Storable a) => Convertible a (Vector a) where
  convert a = Vector.fromList [a]

instance (Storable a) => Convertible (Vector a) [a] where
  convert = Vector.toList

--instance (Convertible Bool b, Storable b) => Convertible BV (Vector b) where
--  convert = convert . BV.toBits

class (Convertible a b, Convertible b a) => Isomorphic a b

