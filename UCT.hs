module UCT where

import Game (Game, Heuristic)
import qualified Game

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Data.Maybe (catMaybes)

import Utils (listToIntMap, maximumByKey, iterateN, toVector, fromVector)

data Node a s =
  Node {
    state     :: s,
    visits    :: Double,
    value     :: Vector Double,
    children  :: IntMap (Node a s)
  }

agent :: Game a s => Node a s -> a
agent node = Game.agent $ state node

getValue node = \a -> (fromVector $ value node) a / (visits node)

listChildren = IntMap.elems . children
getChild node n = (children node) IntMap.! n

bestChild node = maximumByKey (\child -> (getValue child) (agent node)) (listChildren node)

initNode heuristic state =
  Node {
    state = state,
    visits = 1,
    value = toVector $ heuristic state,
    children = listToIntMap $ map (initNode heuristic) (Game.actions state)
  }

pickChild node = maximumByKey
  (\(_, child) -> sqrt (2 * log (visits node) / visits child) + getValue child (agent node))
  (IntMap.assocs $ children node)

explore node
  | Game.terminal (state node) = let
      newVisits = visits node + 1
      newValue = Vector.map (\v -> v / visits node * newVisits) (value node)
      in node {visits = newVisits, value = newValue}
--  | (visits node) == 1 = node {visits = 2, value = value $ explore (bestChild node)}
  | otherwise = let 
      (index, toExplore) = pickChild node
      explored = explore toExplore
      newChildren = IntMap.insert index explored (children node)
      listNewChildren = IntMap.elems newChildren
      --newValue = maximumByKey ($ agent node) (map value listNewChildren)
      newVisits = sum (map visits listNewChildren)
      --newVisits = visits node + 1
      newValue = foldr1 (Vector.zipWith (+)) (map value $ listNewChildren)
    in node {visits = newVisits, value = newValue, children = newChildren}

--uct :: (Game a s) => Int -> Heuristic a s -> s -> Node a s
uct n heuristic = (iterateN n explore) . (initNode heuristic)

--uctPlayer :: (Game a s) => Int -> Heuristic a s -> s -> s
uctPlayer n heuristic state_ = (state . bestChild) $ uct n heuristic state_
