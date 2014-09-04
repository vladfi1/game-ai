module Tree where

import Data.Monoid (Monoid, (<>))

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Prelude hiding (lookup)

data RangeTree a = Node {
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

