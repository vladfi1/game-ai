module UCT where

import Game

import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as IntMap

import Utils (listToIntMap, maximumByKey, iterateN)

data Node a b =
  Node {
    state     :: (a, b),
    visits    :: Double,
    value     :: a -> Double,
    children  :: IntMap (Node a b)
  }

agent :: Node a b -> a
agent node = fst $ state node

bestChild :: Node a b -> Node a b
bestChild node = maximumByKey (\child -> (value child) (agent node)) (IntMap.elems (children node))

initNode :: Game a b => Heuristic a b -> (a, b) -> Node a b
initNode heuristic state =
  Node {
    state = state,
    visits = 1,
    value = heuristic state,
    children = listToIntMap (map (initNode heuristic) (actions state))
  }

explore :: Game a b => Node a b -> Node a b
explore node
  | terminal (state node) = node
  | (visits node) == 1 = node {visits = 2, value = value (bestChild node)}
  | otherwise =
      node {
        visits = visits node + 1,
        value = maximumByKey ($ agent node) [value node, value explored],
        children = IntMap.insert index explored (children node)
      }
      where  
        (index, toExplore) = maximumByKey
          (\(index, child) -> sqrt (2 * log (visits node) / visits child) + (value child) (agent node))
          (IntMap.assocs (children node))
        explored = explore toExplore
        
uct :: (Game a b) => Int -> Heuristic a b -> (a, b) -> (a, b)
uct n heuristic = (state . bestChild) . (iterateN n explore) . (initNode heuristic)
