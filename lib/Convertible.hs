{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Convertible where

import Data.Foldable

import Data.Packed.Vector (Vector)
import qualified Data.Packed.Vector as Vector
import Foreign.Storable

class Convertible a b where
  convert :: a -> b

instance Convertible a a where
  convert = id

instance (Monad m) => Convertible a (m a) where
  convert = return

instance (Functor f, Convertible a b) => Convertible (f a) (f b) where
  convert = fmap convert

instance (Integral a, Num b) => Convertible a b where
  convert = fromIntegral

instance (Foldable f, Convertible a b, Storable b) => Convertible (f a) (Vector b) where
  convert = Vector.fromList . convert . toList

class (Convertible a b, Convertible b a) => Isomorphic a b

