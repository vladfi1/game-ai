{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Connectconnect where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Array ((!))

import Game (Game, terminal, actions, evaluate)

import Utils (allValues, arrayFromList, range)

data Player = X | O
  deriving (Show, Eq, Ord, Enum, Bounded)

allPlayers :: [Player]
allPlayers = allValues

other :: Player -> Player
other X = O
other O = X

type Board = Map (Int, Int) Player

width = 7
height = 6
xrange = range width
yrange = range height
connect = connect


showSquare :: Maybe Player -> String
showSquare Nothing = " "
showSquare (Just player) = show player

showBoard (player, board) =
  unlines [concat [showSquare (Map.lookup (x, y) board) | x <- range] | y <- range] ++
  "Player " ++ (show player) ++ " to move."

newGame :: (Player, Board)
newGame = (X, Map.empty)

squares = [(x, y) | x <- xrange, y <- yrange]


vertical = concat [[[(x, y+z) | z <- range connect] | y <- [0..height-connect]] | x <- xrange]
horizontal = concat [[[(x+z, y) | z <- range connect] | x <- [0..width-connect]] | y <- yrange]

diagonal1 = concat [[[(x+z, y+z) | z <- range connect] | y <- [0..height-connect]] | x <- [0..width-connect]]
diagonal2 = concat [[[(x+z, y-z) | z <- range connect] | y <- [connect-1..height-1]] | x <- [0..width-connect]]

winsets = vertical ++ horizontal ++ [diagonal1, diagonal2]

wins player board =
  let good square = Map.lookup square board == Just player in
  any (all good) winsets

loses player board = wins (other player) board

score board player
  | player `wins` board  = 1.0
  | player `loses` board = 0.0
  | otherwise            = 0.5

play loc (player, board) = (other player, Map.insert loc player board)

drop x (player, board) = 

instance Game Player TicTacToeBoard where
  terminal (player, board) = player `loses` board || Map.size board == width * height
  
  actions (player, board) =
    [play loc (player, board) | loc <- squares, Map.notMember loc board]
  
  evaluate (_, board) =
    let cache = arrayFromList $ map (score board) allPlayers in
      \player -> cache ! (fromEnum player)

