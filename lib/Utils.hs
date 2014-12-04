module Utils where

import Data.List (maximumBy)
import Data.Ord (comparing)

import qualified Data.Vector as V

import Data.Vector.Generic (Vector, (//))
import qualified Data.Vector.Generic as Vector

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

modifyList :: Int -> (a -> a) -> [a] -> [a]
modifyList _ _ [] = error "Can't update empty list!"
modifyList 0 f (a:as) = (f a):as
modifyList n f (a:as) = a : modifyList (n-1) f as

modifyVector :: (Vector v a) => v a -> [(Int, a -> a)] -> v a
modifyVector vec ifs = vec // [(i, f (vec Vector.! i)) | (i, f) <- ifs]

toVector :: (Bounded a, Enum a, Vector v b) => (a -> b) -> v b
toVector f = Vector.fromList $ map f allValues

fromVector :: (Bounded a, Enum a, Vector v b) => v b -> a -> b
fromVector v = (v Vector.!) . fromEnum

mem :: (Bounded a, Enum a) => (a -> b) -> (a -> b)
mem = fromVector . toVector'
  where toVector' = toVector :: (Bounded a, Enum a) => (a -> b) -> V.Vector b
        

fromMap :: (Ord k) => Map k a -> k -> a
fromMap = (Map.!)

invert :: (Ord b) => [(a, [b])] -> [(b, [a])]
invert packed =
  let unpacked = concat [[(a, b) | b <- bs] | (a, bs) <- packed]
      bToAs = foldl (\m (a, b) -> Map.insert b (a : Map.findWithDefault [] b m) m) Map.empty unpacked
    in Map.assocs bToAs

