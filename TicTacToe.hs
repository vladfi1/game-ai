{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TicTacToe where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Array ((!))

import Game (Game, terminal, actions, evaluate)

import Utils (allValues, arrayFromList)

data Player = X | O
  deriving (Show, Eq, Ord, Enum, Bounded)

allPlayers :: [Player]
allPlayers = allValues

other :: Player -> Player
other X = O
other O = X

type TicTacToeBoard = Map (Int, Int) Player

size = 3
range = [1..size]

showSquare :: Maybe Player -> String
showSquare Nothing = " "
showSquare (Just player) = show player

showBoard (player, board) =
  unlines [concat [showSquare (Map.lookup (x, y) board) | x <- range] | y <- range] ++
  "Player " ++ (show player) ++ " to move."

newGame :: (Player, TicTacToeBoard)
newGame = (X, Map.empty)

squares = [(x, y) | x <- range, y <- range]
vertical = [[(x, y) | y <- range] | x <- range]
horizontal = [[(x, y) | x <- range] | y <- range]
diagonal1 = [(x, y) | x <- range, let y = x]
diagonal2 = [(x, y) | x <- range, let y = size + 1 - x]
winsets = vertical ++ horizontal ++ [diagonal1, diagonal2]

wins player board =
  let good square = Map.lookup square board == Just player in
  any (all good) winsets

loses player board = wins (other player) board

score board player = 
  if player `wins` board then 1.0
  else if player `loses` board then 0.0
  else 0.5

play loc (player, board) = (other player, Map.insert loc player board)

instance Game Player TicTacToeBoard where
  terminal (player, board) = player `loses` board || Map.size board == size * size
  
  actions (player, board) =
    [play loc (player, board) | loc <- squares, Map.notMember loc board]
  
  evaluate (_, board) =
    let cache = arrayFromList $ map (score board) allPlayers in
      \player -> cache ! (fromEnum player)
