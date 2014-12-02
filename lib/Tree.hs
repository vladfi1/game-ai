{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
--{-# LANGUAGE NoImplicitPrelude #-}

module Tree where

import Data.Monoid (Monoid, (<>))

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Control.Monad
import Control.Applicative
import Data.Foldable
import Data.Traversable

import qualified Algebra.Ring as Ring

import MonadJoin

class Tree t where
  isNode :: t a -> Bool
  isLeaf :: t a -> Bool
  
  isNode = not . isLeaf
  isLeaf = not . isNode
  
  getLeft :: t a -> t a
  getRight :: t a -> t a
  getValue :: t a -> a

data RangeTree a =
  Node {
    size  :: Int,
    val   :: a,
    left  :: RangeTree a,
    right :: RangeTree a
  } | Leaf a

fromVector :: Monoid a => Vector a -> RangeTree a
fromVector vector =
  if len == 1 then Leaf $ Vector.head vector else
    let (leftVector, rightVector) = Vector.splitAt (quot len 2) vector
        left  = fromVector leftVector
        right = fromVector rightVector
    in Node {
      size = len,
      val = val left <> val right,
      left = left,
      right = right
    }
  where len = Vector.length vector

adjust :: Monoid a => (a -> a) -> Int -> RangeTree a -> RangeTree a
adjust f _ (Leaf a) = Leaf (f a)
adjust f index node =
  let half = quot (size node) 2 in
  if index < half
    then let newLeft = adjust f index (left node) in
      node {
        val = val newLeft <> val (right node),
        left = newLeft
      }
    else let newRight = adjust f (index - half) (right node) in
      node {
        val = val (left node) <> val newRight,
        right = newRight
      }

update :: Monoid a => a -> Int -> RangeTree a -> RangeTree a
update a = adjust (const a)

data TreeW w a =
  NodeW {
    weight :: !w,
    leftW  :: TreeW w a,
    rightW :: TreeW w a
  } |
  LeafW {
    weight :: !w,
    valueW :: !a
  }
  deriving (Functor, Foldable, Traversable)

instance (Ring.C w) => Monad' (TreeW w) where
  return' = LeafW Ring.one 
  join' NodeW {weight, leftW, rightW} = NodeW weight (join' leftW) (join' rightW)
  join' LeafW {weight, valueW} = valueW {weight}

--standard
instance (Ring.C w) => Monad (TreeW w) where
  return = return'
  (>>=) = bind'

--standard
instance (Ring.C w) => Applicative (TreeW w) where
  pure = return
  (<*>) = ap


