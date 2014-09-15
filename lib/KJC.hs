{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module KJC where

import TwoPlayer

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Vector (Vector)
import qualified Data.Vector

import Data.Maybe (isJust)

import Debug.Trace

import Grid (Square)
import qualified Grid
import qualified Utils

import Game (Game, agent, terminal, actions, evaluate)

width = 6
height = 6
dims = (width, height)
xrange = Utils.range width
yrange = Utils.range height

squares = Grid.squares dims
numSquares = length squares
grid = Grid.grid dims
neighbors = Utils.fromMap grid
degrees = Utils.fromMap (Map.map length grid)

data KJCState = KJCState {
--  total :: Int,
  player :: Player,
  values :: Map Square Int,
--  owners :: Map Square Player,
--  owned  :: Vector (Set Square),
  owned  :: Player -> (Set Square),
  winner :: Maybe Player
}

newGame = KJCState {
--  total = 0,
  player = X,
  values = Map.fromList [(square, 0) | square <- squares],
--  owners = Map.empty,
--  owned  = Utils.toVector (const Set.empty),
  owned  = const Set.empty,
  winner = Nothing
}

assign player square owned = Utils.mem (\p ->
  (if p == player then Set.insert else Set.delete) square (owned p))

check state @ KJCState {player, owned} state' =
  if Set.size (owned player) == numSquares
    then state {winner = Just player}
    else state'

increment :: KJCState -> [Square] -> KJCState

increment state @ KJCState {player} [] =
  check state $
  state {player = other player}

increment state @ KJCState {player, values, owned} (next:rest) =
  check state $
  let value = values Map.! next + 1
      owned' = assign player next owned
  in if value == degrees next
    then increment state {
      values = Map.insert next 0 values,
      owned = owned'
    } (neighbors next ++ rest)
    else increment state {
      values = Map.insert next value values,
      owned = owned'
    } rest

play state square = increment state [square]

playable KJCState {player, owned} =
  filter (\square -> Set.notMember square banned) squares
    where banned = owned (other player)

instance Game Player KJCState where
  agent = player
  terminal = isJust . winner
  actions state = map (play state) (playable state)
  evaluate state = fromIntegral . Set.size . (owned state)

instance Show KJCState where
  show KJCState {player, values} =
    unlines [concat [show $ values Map.! (x, y) | x <- xrange] | y <- reverse yrange]
    ++ "Player " ++ (show $ player) ++ " to move."
  
