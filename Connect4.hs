{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Connect4 where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap


import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vector

import Data.Maybe (catMaybes, isJust)

import Game (Game, agent, terminal, actions, evaluate)

import Utils (allValues, range, listToIntMap, enumerate, mem, toVector, fromVector)
import qualified Utils

data Player = X | O
  deriving (Show, Eq, Ord, Enum, Bounded)

allPlayers :: [Player]
allPlayers = allValues

other :: Player -> Player
other X = O
other O = X

type Square = (Int, Int)
type Board = Map Square Player
type WinSet = Int

data Connect4Board =
  Connect4Board {
    player  :: Player,
    squares :: Board,
    winner  :: Maybe Player
  }

width = 7
height = 6
xrange = range width
yrange = range height
connect = 4

deltas = [(1, 0), (0, 1), (1, 1), (1, -1)]

vertical = concat [[[(x, y+z) | z <- range connect] | y <- [0..height-connect]] | x <- xrange]
horizontal = concat [[[(x+z, y) | z <- range connect] | x <- [0..width-connect]] | y <- yrange]
diagonal1 = concat [[[(x+z, y+z) | z <- range connect] | y <- [0..height-connect]] | x <- [0..width-connect]]
diagonal2 = concat [[[(x+z, y-z) | z <- range connect] | y <- [connect-1..height-1]] | x <- [0..width-connect]]
winsets = concat [vertical, horizontal, diagonal1, diagonal2] :: [[Square]]

vectorWinSets = Vector.fromList winsets

getWinSet :: WinSet -> [Square]
getWinSet = (vectorWinSets Vector.!)

inverseWinSets :: Map Square [WinSet]
inverseWinSets = Map.fromList $ Utils.invert (enumerate winsets)

getWinSets :: Square -> [[Square]]
getWinSets square = [getWinSet winset | winset <- inverseWinSets Map.! square]

newGame = Connect4Board {
  player  = X,
  squares = Map.empty,
  winner  = Nothing
}

check :: Board -> Player -> Square -> Maybe Player
check board player square =
  let check' winset = all (== Just player) [Map.lookup square board | square <- winset] in
  if any check' (getWinSets square)
    then Just player
    else Nothing

play :: Connect4Board -> Square -> Connect4Board
play board loc =
  let
    previous   = player board
    newSquares = Map.insert loc previous (squares board)
  in Connect4Board {
    player  = other previous,
    squares = newSquares,
    winner  = check newSquares previous loc
  }

dropLoc (x,y) squares
  | y == height                 = Nothing
  | Map.notMember (x,y) squares = Just (x, y)
  | otherwise                   = dropLoc (x, y+1) squares

dropCol board x = do
  loc <- dropLoc (x, 0) (squares board)
  return (play board loc)

score board player = 
  case winner board of
    Just p  -> if p == player then 1.0 else 0.0
    Nothing -> 0.5

instance Game Player Connect4Board where
  agent = player

  terminal board = (isJust $ winner board) || Map.size (squares board) == width * height
  
  actions board = catMaybes [dropCol board x | x <- xrange]
  
  evaluate = score

showSquare :: Maybe Player -> String
showSquare Nothing = " "
showSquare (Just player) = show player

getSquare loc board = Map.lookup loc (squares board)

instance Show Connect4Board where
  show board =
    unlines [concat [showSquare (getSquare (x, y) board) | x <- xrange] | y <- reverse yrange] ++
    "Player " ++ (show $ player board) ++ " to move."

