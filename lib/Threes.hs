--{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NamedFieldPuns #-}

module Threes where

import Prelude hiding (Left, Right)

--import qualified Data.Map
import Data.Maybe (catMaybes)

import Data.Vector (Vector, (!), (//), indexed)
import qualified Data.Vector as Vector

import Data.Matrix hiding ((!))
import qualified Data.Matrix as Matrix

import Data.Monoid
import Data.Foldable (foldMap)

import qualified Control.Monad.Random as Random
import Control.Monad.State hiding (state)

import Utils (range, enumerate, allValues, modifyList)
import Discrete
--import Instances
import NewGame
import OnePlayer

type Tile = Int
type Row = Vector Tile
type Grid = Matrix Tile

width = 4
height = 4

combines :: Tile -> Tile -> Bool
combines 0 x = x /= 0
combines 1 x = x == 2
combines 2 x = x == 1
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
--fromRows rows = foldr1 (<->) (map rowVector rows)

data Direction = Up | Down | Left | Right
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

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

pushGrid :: Direction -> Grid -> ([Row], [Int])
pushGrid dir grid = (pushed, open)
  where rows = disassemble dir grid
        push (i, row) (is, rs) =
          case pushRow row of
            Nothing -> (is, row : rs)
            Just r -> (i : is, r : rs)
        (open, pushed) = foldr push ([], []) (enumerate rows)

placeTile :: Tile -> Row -> Row
placeTile tile = (// [(0, tile)])

type RNG = Vector Int
rngSize = 4
newRNG = Vector.fromList [rngSize, rngSize]

pick1or2 :: (Num w, MonadDiscrete w m) => StateT RNG m Tile
pick1or2 = do
  rng <- get
  let indices = enumerate . (map fromIntegral) $ Vector.toList rng
  index <- lift $ sample indices
  let rng' = rng // [(index, (rng ! index) - 1)]
  put (if Vector.sum rng' == 0 then newRNG else rng')
  return (index + 1)

pickTile :: (Fractional w, MonadDiscrete w m) => Int -> StateT RNG m Tile
pickTile maxTile = do
  oneOrTwo <- lift $ bernoulli 0.2
  if oneOrTwo then pick1or2
    else lift $ sample [(3, 0.75), (6, 0.25)]

data ThreesState =
  ThreesDir {
    grid :: Grid,
    nextTile :: Tile,
    rng :: RNG
  } |
  ThreesLoc {
    grid :: Grid,
    direction :: Direction,
    rows :: [Row],
    open :: [Int],
    nextTile :: Tile,
    rng :: RNG
  } |
  ThreesNum {
    grid :: Grid,
    rng :: RNG
  }

threesDir :: ThreesState -> Direction -> Maybe ThreesState
threesDir ThreesDir {grid, nextTile, rng} dir =
  if null open then Nothing else
    Just $ ThreesLoc {
      grid = reassemble dir rows,
      direction = dir,
      rows = rows,
      open = open,
      nextTile = nextTile,
      rng = rng
    }
  where (rows, open) = pushGrid dir grid

threesLoc ThreesLoc {direction, rows, open, nextTile, rng} = do
  index <- uniform open
  let newRows = modifyList index (placeTile nextTile) rows
  let grid = reassemble direction newRows
  return $ ThreesNum {grid, rng}

threesNum ThreesNum {grid, rng} = do
  let maxTile = maximum (Matrix.toList grid)
  (tile, rng') <- runStateT (pickTile maxTile) rng
  return $ ThreesDir {
    grid = grid,
    nextTile = tile,
    rng = rng'
  }
  
threesToGame :: ThreesState -> GameState Player Direction ThreesState
threesToGame state @ ThreesDir {grid, nextTile, rng} =
  Player {
    state = state,
    agent = You,
    actions = catMaybes [fmap (\s -> (dir, threesToGame s)) (threesDir state dir) | dir <- allValues]
  }

threesToGame state @ ThreesLoc {} =
  Nature {
    state = state,
    nature = liftM threesToGame (threesLoc state)
  }

threesToGame state @ ThreesNum {} =
  Nature {
    state = state,
    nature = liftM threesToGame (threesNum state)
  }

instance Show ThreesState where
  show ThreesDir {grid, nextTile} =
    (show grid) ++ (show nextTile) ++ " is next.\n"

newGrid = Matrix.fromLists [[3, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]
newGame = threesToGame $ ThreesNum {grid = newGrid, rng = newRNG}

scoreTile :: Tile -> Int
scoreTile n = n

scoreGrid :: Grid -> Int
scoreGrid = getSum . (foldMap $ Sum . scoreTile)

countEmpty :: Grid -> Int
countEmpty = getSum . (foldMap $ Sum . isZero)
  where isZero 0 = 1
        isZero _ = 0

basicHeuristic :: GameState Player Direction ThreesState -> Player -> Double
basicHeuristic game You =
  fromIntegral $ (scoreGrid g) + 10 * (countEmpty g)
  where g = grid $ state game

