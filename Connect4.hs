{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Connect4 where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Vector (Vector, (!), (//))
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
type WinSet = Int
type Stat = Player -> Int

data Connect4Board =
  Connect4Board {
    player  :: Player,
    squares :: Map Square Player,
    stats   :: Map WinSet Stat,
    winner  :: Maybe Player
  }

width = 7
height = 6
xrange = range width
yrange = range height
connect = 4

vertical = concat [[[(x, y+z) | z <- range connect] | y <- [0..height-connect]] | x <- xrange]
horizontal = concat [[[(x+z, y) | z <- range connect] | x <- [0..width-connect]] | y <- yrange]
diagonal1 = concat [[[(x+z, y+z) | z <- range connect] | y <- [0..height-connect]] | x <- [0..width-connect]]
diagonal2 = concat [[[(x+z, y-z) | z <- range connect] | y <- [connect-1..height-1]] | x <- [0..width-connect]]
winsets = concat [vertical, horizontal, diagonal1, diagonal2] :: [[Square]]

inverseWinSets :: Map Square [WinSet]
inverseWinSets = Map.fromList $ Utils.invert (enumerate winsets)

emptyStat = const 0

newGame = Connect4Board {
  player  = X,
  squares = Map.empty,
  stats   = Map.fromList $ enumerate [emptyStat | _ <- winsets],
  winner  = Nothing
}

update :: Player -> Stat -> Stat
update player stat =
  fromVector $ v // [(i, 1 + stat player)]
  where
    i = fromEnum player
    v = toVector stat

wins player stat = stat player == connect

play loc board =
  let
    previous = player board
    newStats = foldr (Map.adjust (update previous)) (stats board) (inverseWinSets Map.! loc)
  in Connect4Board {
    player  = other previous,
    squares = Map.insert loc previous (squares board),
    stats   = newStats,
    winner  = if any (wins previous) [newStats Map.! winset | winset <- inverseWinSets Map.! loc]
                then Just previous else Nothing
  }

dropLoc (x,y) squares
  | y == height                 = Nothing
  | Map.notMember (x,y) squares = Just (x, y)
  | otherwise                   = dropLoc (x, y+1) squares

dropCol x board = do
  loc <- dropLoc (x, 0) (squares board)
  return (play loc board)

score board player = 
  case winner board of
    Just p  -> if p == player then 1.0 else 0.0
    Nothing -> 0.5

instance Game Player Connect4Board where
  agent = player

  terminal board = (isJust $ winner board) || Map.size (squares board) == width * height
  
  actions board = catMaybes [dropCol x board | x <- xrange]
  
  evaluate board = mem (score board)

showSquare :: Maybe Player -> String
showSquare Nothing = " "
showSquare (Just player) = show player

getSquare loc board = Map.lookup loc (squares board)

instance Show Connect4Board where
  show board =
    unlines [concat [showSquare (getSquare (x, y) board) | x <- xrange] | y <- reverse yrange] ++
    "Player " ++ (show $ player board) ++ " to move."

