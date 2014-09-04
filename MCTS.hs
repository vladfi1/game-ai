module MCTS where

import Game (Game, Heuristic)
import qualified Game

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Maybe (catMaybes)

import Utils (listToIntMap, iterateN)

data Node a v = Node {
--    state     :: s,
    agent    :: a,
    value    :: v,
    children :: IntMap (Node a v)
  }

--agent :: Game a s => Node s v -> a
--agent node = Game.agent $ state node

listChildren = IntMap.elems . children
getChild node n = (children node) IntMap.! n

--bestChild :: Ord v 
--bestChild node = maximumByKey (\child -> (getValue child) (agent node)) (listChildren node)

initNode heuristic state =
  Node {
--    state = state,
    agent = Game.agent state,
    value = heuristic state,
    children = listToIntMap $ map (initNode heuristic) (Game.actions state)
  }

data Strategy a v = Strategy {
  pick        :: IntMap v -> Int,
  incorporate :: IntMap v -> Int -> v
}

explore :: Game a s => Strategy v -> Node s v -> Node s v
explore strategy node
  | Game.terminal (state node) = node
  | otherwise = let
      oldChildren = (children node)
      index = (pick strategy) $ IntMap.map value oldChildren
      explored = explore strategy $ oldChildren IntMap.! index
      newChildren = IntMap.insert index explored oldChildren
      newValue = (incorporate strategy) (IntMap.map value newChildren) index
    in node {value = newValue, children = newChildren}

mcts :: (Game a s) => Strategy v -> Int -> (s -> v) -> s -> Node s v
mcts strategy n heuristic = (iterateN n $ explore strategy) . (initNode heuristic)

