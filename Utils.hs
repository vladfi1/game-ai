module Utils where

import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Array (Array, array)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Map (Map)
import qualified Data.Map as Map

maximumByKey :: Ord b => (a -> b) -> [a] -> a
maximumByKey key = maximumBy (comparing key)

iterateN :: Int -> (a -> a) -> a -> a
iterateN n f a =
  if (n == 0) then a else
    iterateN (n-1) f (f a)

range len = [0..len-1]

enumerate :: [a] -> [(Int, a)]
enumerate list = zip (range $ length list) list

listToIntMap :: [a] -> IntMap a
listToIntMap list = IntMap.fromList (enumerate list)

arrayFromList :: [a] -> Array Int a
arrayFromList list = array (0, (length list) - 1) (enumerate list)

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound .. ]

invert :: (Ord b) => [(a, [b])] -> [(b, [a])]
invert packed =
  let unpacked = concat [[(a, b) | b <- bs] | (a, bs) <- packed]
      bToAs = foldl (\m (a, b) -> Map.insert b (a : Map.findWithDefault [] b m) m) Map.empty unpacked
    in Map.assocs bToAs
