module UCT where

import Game (Game, Heuristic)
import qualified Game

import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as IntMap

import Data.Maybe (catMaybes)

import Utils (listToIntMap, maximumByKey, iterateN)

data Node a s =
  Node {
    state     :: s,
    visits    :: Double,
    value     :: a -> Double,
    children  :: IntMap (Node a s)
  }

agent :: Game a s => Node a s -> a
agent node = Game.agent $ state node

listChildren = IntMap.elems . children
getChild node n = (children node) ! n

bestChild :: Game a s => Node a s -> Node a s
bestChild node = maximumByKey (\child -> (value child) (agent node)) (listChildren node)

initNode :: Game a s => Heuristic a s -> s -> Node a s
initNode heuristic state =
  Node {
    state = state,
    visits = 1,
    value = heuristic state,
    children = listToIntMap $ map (initNode heuristic) (Game.actions state)
  }

explore :: Game a b => Node a b -> Node a b
explore node
  | Game.terminal (state node) = node
  | (visits node) == 1 = node {visits = 2, value = value (bestChild node)}
  | otherwise = node {visits = visits node + 1, value = newValue, children = newChildren}
      where
        (index, toExplore) = maximumByKey
          (\(_, child) -> sqrt (2 * log (visits node) / visits child) + (value child) (agent node))
          (IntMap.assocs $ children node)
        explored = explore toExplore
        newChildren = IntMap.insert index explored (children node)
        newValue = maximumByKey ($ agent node) (map value (IntMap.elems newChildren))

verify :: Game a s => Node a s -> Maybe (Node a s)
verify node =
  if visits node == 1 then Nothing else
    case catMaybes $ map verify (listChildren node) of
      first:_ -> Just first
      [] ->
        if value node (agent node) == value (bestChild node) (agent node)
          then Nothing
          else Just node

uct :: (Game a s) => Int -> Heuristic a s -> s -> Node a s
uct n heuristic = (iterateN n explore) . (initNode heuristic)

uctPlayer :: (Game a s) => Int -> Heuristic a s -> s -> s
uctPlayer n heuristic state_ = (state . bestChild) $ uct n heuristic state_
