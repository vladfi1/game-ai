module Utils where

import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad ((>=>))

import qualified Numeric

decimals n f = Numeric.showFFloat (Just n) f ""

inRange :: Ord a => (a, a) -> a -> Bool
inRange (lower, upper)  = \value -> lower <= value && value < upper

maximumByKey :: Ord b => (a -> b) -> [a] -> a
maximumByKey key = maximumBy (comparing key)

iterateN :: Int -> (a -> a) -> a -> a
iterateN 0 _ a = a
iterateN n f a = iterateN (n-1) f (f a)

iterateM :: (Monad m) => Int -> (a -> m a) -> a -> m a
iterateM 0 _ = return
iterateM n f = (iterateM (n-1) f) >=> f

range len = [0..len-1]

enumerate :: [a] -> [(Int, a)]
enumerate list = zip (range $ length list) list

listToIntMap :: [a] -> IntMap a
listToIntMap list = IntMap.fromList (enumerate list)

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound .. ]

toVector :: (Bounded a, Enum a) => (a -> b) -> Vector b
toVector f = Vector.fromList $ map f allValues

fromVector :: (Bounded a, Enum a) => Vector b -> a -> b
fromVector v = (v Vector.!) . fromEnum

mem :: (Bounded a, Enum a) => (a -> b) -> (a -> b)
mem = fromVector . toVector

invert :: (Ord b) => [(a, [b])] -> [(b, [a])]
invert packed =
  let unpacked = concat [[(a, b) | b <- bs] | (a, bs) <- packed]
      bToAs = foldl (\m (a, b) -> Map.insert b (a : Map.findWithDefault [] b m) m) Map.empty unpacked
    in Map.assocs bToAs

