module MCTS where

import Game (Game, Heuristic)
import qualified Game

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Maybe (catMaybes)

import Utils (listToIntMap, iterateN)

data Node s v = Node {
    state     :: s,
--    terminal :: Bool,
--    agent    :: a,
    value    :: v,
    children :: IntMap (Node s v)
  }

agent :: Game a s => Node s v -> a
agent node = Game.agent $ state node

listChildren = IntMap.elems . children
getChild node n = (children node) IntMap.! n

--bestChild :: Ord v 
--bestChild node = maximumByKey (\child -> (getValue child) (agent node)) (listChildren node)

initNode heuristic state =
  Node {
    state = state,
--    terminal = Game.terminal state
--    agent = Game.agent state,
    value = heuristic state,
    children = listToIntMap $ map (initNode heuristic) (Game.actions state)
  }

data Strategy s v = Strategy {
  pick        :: Node s v -> Int,
  terminal    :: Node s v -> Node s v,
  incorporate :: Node s v -> Int -> v
}

explore :: Game a s => Strategy s v -> Node s v -> Node s v
explore strategy node
  | Game.terminal (state node) = terminal strategy node
  | otherwise = let
      oldChildren = (children node)
      index = pick strategy node
      newChildren = IntMap.adjust (explore strategy) index oldChildren
      node' = node {children = newChildren}
      newValue = (incorporate strategy) node' index
    in node' {value = newValue}

mcts :: (Game a s) => Strategy s v -> Int -> (s -> v) -> s -> Node s v
mcts strategy n heuristic = (iterateN n $ explore strategy) . (initNode heuristic)

