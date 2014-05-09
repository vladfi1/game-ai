module Utils where

import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.IntMap (IntMap, fromList)
import Data.Array (Array, array)

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
listToIntMap list = fromList (enumerate list)

arrayFromList :: [a] -> Array Int a
arrayFromList list = array (0, (length list) - 1) (enumerate list)

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound .. ]
