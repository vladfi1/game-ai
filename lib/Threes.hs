module Threes where

import Prelude hiding (Left, Right)

import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vector

import Data.Matrix hiding ((!))
import qualified Data.Matrix as Matrix

import qualified Control.Monad.Random as Random

import Utils (range, enumerate)

type Tile = Int
type Row = Vector Tile
type Grid = Matrix Tile

width = 4
height = 4

combines :: Tile -> Tile -> Bool
combines 0 x = x /= 0
combines 1 2 = True
combines 2 1 = True
combines x y = x == y

pushIndex :: Row -> Maybe Int
pushIndex row = tryIndex (Vector.length row - 1)
  where tryIndex i | i == 0 = Nothing
                   | combines (row ! i) (row ! (i-1)) = Just i
                   | otherwise = tryIndex (i - 1)

pushRow :: Row -> Maybe Row
pushRow row =
  (pushIndex row) >>= \index ->
    let updates = (0, 0) : (index, row ! index + row ! (index - 1)) : [(i, row ! (i-1)) | i <- [1..index-1]] in
    return $ row // updates

getRows :: Matrix a -> [Vector a]
getRows matrix = [getRow i matrix | i <- [1 .. nrows matrix]]

fromRows :: [Vector a] -> Matrix a
fromRows = Matrix.fromLists . (map Vector.toList)

data Direction = Up | Down | Left | Right deriving (Show, Eq, Ord, Enum, Bounded)

transposes Down = True
transposes Up = True
transposes _ = False

reverses Left = True
reverses Up = True
reverses _ = False

disassemble :: Direction -> Grid -> [Row]
disassemble dir grid = let
  transposed = if transposes dir then transpose grid else grid
  rows = getRows transposed
  reversed = if reverses dir then map Vector.reverse rows else rows
  in reversed

reassemble :: Direction -> [Row] -> Grid
reassemble dir rows = let
  reversed = if reverses dir then map Vector.reverse rows else rows
  mat = fromRows reversed
  transposed = if transposes dir then transpose mat else mat
  in transposed

type ThreesDir = Grid
data ThreesLoc = ThreesLoc Direction [Int] [Row]

pushGrid :: Direction -> Grid -> ThreesLoc
pushGrid dir grid = ThreesLoc dir open pushed
  where rows = disassemble dir grid
        push (i, row) (is, rs) = case pushRow row of
          Nothing -> (is, row : rs)
          Just r -> (i : is, r : rs)
        (open, pushed) = foldr push ([], []) (enumerate rows)

data ThreesNum = ThreesNum Direction Int Row

pickLoc :: (MonadRandom m) => ThreesLoc -> m ThreesNum
pickLoc ThreesLoc dir is rows = do
  i 


type RNG = Vector Int

newRNG = Vector.fromList [10, 10]

pick1or2 :: (MonadRandom m) => RNG -> m (RNG, Tile)
pick1or2 rng = do
  let weights = Vector.toList (indexed rng)
  index <- Random.fromList weights
  let rng' = rng // (index, (rng ! index) - 1)
  let rng'' = if Vector.sum rng' == 0 then newRNG else rng'
  return (rng'', index + 1)

