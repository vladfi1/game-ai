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

import Grid (Square)
import qualified Grid
import qualified Utils

import Game (Game, agent, terminal, actions, evaluate)

width = 6
height = 6
ranges = (width, height)

squares = Grid.squares ranges
grid = Grid.grid ranges
neighbors = Utils.fromMap grid
degrees = Utils.fromMap (Map.map length grid)

data KJCState = KJCState {
  player :: Player,
  values :: Map Square Int,
--  owners :: Map Square Player,
--  owned  :: Vector (Set Square),
  owned  :: Player -> (Set Square),
  winner :: Maybe Player
}

newGame = KJCState {
  player = X,
  values = Map.fromList [(square, 0) | square <- squares],
--  owners = Map.empty,
--  owned  = Utils.toVector (const Set.empty),
  owned  = const Set.empty,
  winner = Nothing
}

assign player square owned = Utils.mem (\p ->
  (if p == player then Set.insert else Set.delete) square (owned p))

increment :: KJCState -> [Square] -> KJCState
increment state [] = state
increment state @ KJCState {player, values, owned} (next:rest) =
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

play state square =
  let state' @ KJCState {player, owned} = increment state [square] in
    state' {
      player = other player,
      winner = if Set.size (owned player) == width * height then Just player else Nothing
    }

playable KJCState {player, owned} = filter (\square -> Set.notMember square banned) squares
  where banned = owned (other player)

instance Game Player KJCState where
  agent = player
  terminal = isJust . winner
  actions state = map (play state) (playable state)
  evaluate state = fromIntegral . Set.size . (owned state)

