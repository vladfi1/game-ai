{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Connect4 (module Connect4, module TwoPlayer) where

import Prelude hiding ((+), (-), (*), sum, negate)
import NumericPrelude

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Data.Maybe (catMaybes, isJust)

import TwoPlayer
import Game (Game, agent, terminal, actions, evaluate)

import Utils (allValues, range, listToIntMap, enumerate)
import qualified Utils

import Control.Monad.State

type Square = (Int, Int)
type Column = IntMap Player
type Board =  IntMap Column

getSquare :: Square -> Board -> Maybe Player
getSquare (x, y) = (IntMap.lookup x) >=> (IntMap.lookup y)

data Connect4Board =
  Connect4Board {
    player  :: Player,
    squares :: Board,
    winner  :: Maybe Player
  }

count :: Connect4Board -> Int
count board = sum $ map IntMap.size (IntMap.elems $ squares board)

width = 7
height = 6
xrange = range width
yrange = range height
connect = 4

type Delta = (Int, Int)
deltas :: [Delta]
deltas = [(1, 0), (0, 1), (1, 1), (1, -1)]

probe :: Board -> Player -> Square -> Delta -> Int
probe board player square delta = let
  probe' current n =
    if getSquare current board == Just player
      then probe' (current + delta) (n+1)
      else n
  in probe' (square + delta) 0

biprobe :: Board -> Player -> Square -> Delta -> Int
biprobe board player square delta =
  probe board player square delta + probe board player square (negate delta)
--  where f = probe board player square
--  sum $ map (probe board player square) [delta, negate delta]

newGame = Connect4Board {
  player  = X,
  squares = IntMap.fromList $ enumerate (replicate width IntMap.empty),
  winner  = Nothing
}

check :: Board -> Player -> Square -> Maybe Player
check board player square =
  if any (== connect - 1) (map (biprobe board player square) deltas)
    then Just player
    else Nothing

dropCol :: Column -> Maybe Int
dropCol col =
  if size == height
    then Nothing
    else Just size
  where size = IntMap.size col

play :: Connect4Board -> Int -> Maybe Connect4Board
play board x = do
  let cols = squares board
  let col = (cols IntMap.! x)
  y <- dropCol col
  let previous = player board
  let newCol = IntMap.insert y previous col
  let newSquares = IntMap.insert x newCol cols
  return Connect4Board {
    player  = other previous,
    squares = newSquares,
    winner  = check newSquares previous (x, y)
  }

score board player = 
  case winner board of
    Just p  -> if p == player then 1.0 else 0.0
    Nothing -> 0.5

instance Game Player Connect4Board where
  agent = player

  terminal board = (isJust $ winner board) || count board == width * height
  
  actions board = catMaybes [play board x | x <- xrange]
  
  evaluate = score

showSquare :: Maybe Player -> String
showSquare Nothing = " "
showSquare (Just player) = show player

instance Show Connect4Board where
  show board =
    unlines [concat [showSquare (getSquare (x, y) (squares board)) | x <- xrange] | y <- reverse yrange]
    ++ (concat $ map show (range width))
    ++ "\nPlayer " ++ (show $ player board) ++ " to move."

