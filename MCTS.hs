module MCTS where

import Game (Game, Heuristic)
import qualified Game

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Data.Maybe (catMaybes)

import Utils (listToIntMap, maximumByKey, iterateN, toVector, fromVector)

data Node s v = Node {
    state     :: s,
    value     :: v,
    children  :: IntMap (Node s v)
  }

agent :: Game a s => Node s v -> a
agent node = Game.agent $ state node

listChildren = IntMap.elems . children
getChild node n = (children node) IntMap.! n

--bestChild node = maximumByKey (\child -> (getValue child) (agent node)) (listChildren node)

initNode heuristic state =
  Node {
    state = state,
    value = heuristic state,
    children = listToIntMap $ map (initNode heuristic) (Game.actions state)
  }

data Strategy v = Strategy {
  pick        :: IntMap v -> Int,
  incorporate :: IntMap v -> Int -> v
}

explore :: Game a s => Strategy v -> Node s v -> Node s v
explore strategy node
  | Game.terminal (state node) = node
  | otherwise = let 
      index = (pick strategy) $ IntMap.map value (children node)
      explored = explore strategy $ (children node) IntMap.! index
      newChildren = IntMap.insert index explored (children node)
      newValue = (incorporate strategy) (IntMap.map value newChildren) index
    in node {value = newValue, children = newChildren}

