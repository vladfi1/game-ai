{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Connect4 where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Array (Array, (!), (//))
import Data.Maybe (catMaybes, isJust)

import Game (Game, terminal, actions, evaluate)

import Utils (allValues, arrayFromList, range, listToIntMap, enumerate)
import qualified Utils

data Player = X | O
  deriving (Show, Eq, Ord, Enum, Bounded)

allPlayers :: [Player]
allPlayers = allValues

other :: Player -> Player
other X = O
other O = X

data Connect4Board =
  Connect4Board {
    squares :: Map (Int, Int) Player,
    stats   :: IntMap (Array Int Int),
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
winsets = concat [vertical, horizontal, diagonal1, diagonal2]

inverseWinSets :: Map (Int, Int) [Int]
inverseWinSets = Map.fromList $ Utils.invert (enumerate winsets)

emptyStat = arrayFromList $ map (const 0) allPlayers

newGame = (,) X
  Connect4Board {
    squares = Map.empty,
    stats   = listToIntMap $ map (const emptyStat) winsets,
    winner  = Nothing
  }

update :: Player -> Array Int Int -> Array Int Int
update player stat = stat // [(i, (stat ! i + 1))] where i = fromEnum player

wins player stat = stat ! (fromEnum player) == connect

play loc (player, board) = (,) (other player)
  Connect4Board {
    squares = Map.insert loc player (squares board),
    stats   = newStats,
    winner  = if any (wins player) [newStats IntMap.! winset | winset <- inverseWinSets Map.! loc]
                then Just player else Nothing
  }
  where newStats = foldr (IntMap.adjust (update player)) (stats board) (inverseWinSets Map.! loc)
    

dropLoc (x,y) squares
  | y == height                 = Nothing
  | Map.notMember (x,y) squares = Just (x, y)
  | otherwise                   = dropLoc (x, y+1) squares

dropCol x (player, board) = do
  loc <- dropLoc (x, 0) (squares board)
  return (play loc (player, board))

score board player = 
  case winner board of
    Just p  -> if p == player then 1.0 else 0.0
    Nothing -> 0.5

instance Game Player Connect4Board where
  terminal (player, board) = (isJust $ winner board) || Map.size (squares board) == width * height
  
  actions (player, board) = catMaybes [dropCol x (player, board) | x <- xrange]
  
  evaluate (_, board) =
    let cache = arrayFromList $ map (score board) allPlayers in
      \player -> cache ! (fromEnum player)

showSquare :: Maybe Player -> String
showSquare Nothing = " "
showSquare (Just player) = show player

getSquare loc board = Map.lookup loc (squares board)

showBoard (player, board) =
  unlines [concat [showSquare (getSquare (x, y) board) | x <- xrange] | y <- reverse yrange] ++
  "Player " ++ (show player) ++ " to move."

