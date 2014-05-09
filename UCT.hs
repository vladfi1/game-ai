module UCT where

import Game

import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as IntMap

import Data.Maybe (catMaybes)

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

listChildren = IntMap.elems . children
getChild node n = (children node) ! n

bestChild :: Node a b -> Node a b
bestChild node = maximumByKey (\child -> (value child) (agent node)) (listChildren node)

initNode :: Game a b => Heuristic a b -> (a, b) -> Node a b
initNode heuristic state =
  Node {
    state = state,
    visits = 1,
    value = heuristic state,
    children = listToIntMap $ map (initNode heuristic) (actions state)
  }

explore :: Game a b => Node a b -> Node a b
explore node
  | terminal (state node) = node
  | (visits node) == 1 = node {visits = 2, value = value (bestChild node)}
  | otherwise = node {visits = visits node + 1, value = newValue, children = newChildren}
      where  
        (index, toExplore) = maximumByKey
          (\(_, child) -> sqrt (2 * log (visits node) / visits child) + (value child) (agent node))
          (IntMap.assocs $ children node)
        explored = explore toExplore
        newChildren = IntMap.insert index explored (children node)
        newValue = maximumByKey ($ agent node) (map value (IntMap.elems newChildren))

verify :: Node a b -> Maybe (Node a b)
verify node =
  if visits node == 1 then Nothing else
    case catMaybes $ map verify (listChildren node) of
      first:_ -> Just first
      [] ->
        if value node (agent node) == value (bestChild node) (agent node)
          then Nothing
          else Just node

uct :: (Game a b) => Int -> Heuristic a b -> (a, b) -> Node a b
uct n heuristic = (iterateN n explore) . (initNode heuristic)

uctPlayer :: (Game a b) => Int -> Heuristic a b -> (a, b) -> (a, b)
uctPlayer n heuristic state_ = (state . bestChild) $ uct n heuristic state_
