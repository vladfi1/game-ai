{-# LANGUAGE NamedFieldPuns #-}

module MCTS where

import Game (Game, Heuristic)
import qualified Game

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Maybe (catMaybes)

import Utils (listToIntMap, iterateN)

data Tree s v =
  Node {
    state    :: s,
--    terminal :: Bool,
--    agent    :: a,
    value    :: v,
    children :: IntMap (Tree s v)
  } | Leaf {
    state :: s,
    value :: v
  }

agent :: Game a s => Tree s v -> a
agent node = Game.agent $ state node

listChildren = IntMap.elems . children
getChild node n = (children node) IntMap.! n

--bestChild :: Ord v 
--bestChild node = maximumByKey (\child -> (getValue child) (agent node)) (listChildren node)

initLeaf heuristic state =
  Leaf {
    state = state,
    value = heuristic state
  }

initChildren heuristic state = map (initLeaf heuristic) (Game.actions state)

initNode strategy state = let
  children = initChildren (heuristic strategy) state
  in Node {
    state = state,
    value = incorporate strategy children,
    children = listToIntMap children
  }

data Strategy s v = Strategy {
  heuristic   :: s -> v,
  pick        :: Tree s v -> Int,
  terminal    :: Tree s v -> Tree s v,
  incorporate :: [Tree s v] -> v,
  update      :: Tree s v -> Int -> v
}

explore :: Game a s => Strategy s v -> Tree s v -> Tree s v

explore strategy leaf @ Leaf {state}
  | Game.terminal state = terminal strategy leaf
  | otherwise = initNode strategy state

explore strategy node @ Node {state, value, children} = let
    index = pick strategy node
    newChildren = IntMap.adjust (explore strategy) index children
    node' = node {children = newChildren}
    newValue = update strategy node' index
  in node' {value = newValue}

mcts :: (Game a s) => Strategy s v -> Int -> s -> Tree s v
mcts strategy n = (iterateN n $ explore strategy) . (initLeaf (heuristic strategy))

