{-# LANGUAGE NamedFieldPuns #-}

module MCTS where

import Game (Game, Heuristic)
import qualified Game

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Maybe (catMaybes)

import Control.Monad ((>=>))

import qualified Utils

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

listChildren Node {children} = IntMap.elems children
listChildren _ = []

getChild node n = (children node) IntMap.! n

--bestChild :: Ord v 
--bestChild node = maximumByKey (\child -> (getValue child) (agent node)) (listChildren node)

initLeaf heuristic state = do
  value <- heuristic state
  return $ Leaf {
    state = state,
    value = value
  }

initChildren heuristic state = mapM (initLeaf heuristic) (Game.actions state)

initNode strategy state = do
  children <- initChildren (heuristic strategy) state
  return $ Node {
    state = state,
    value = incorporate strategy children,
    children = Utils.listToIntMap children
  }

data Strategy s m v = Strategy {
  heuristic   :: s -> m v,
  pick        :: Tree s v -> m Int,
  terminal    :: Tree s v -> Tree s v,
  incorporate :: [Tree s v] -> v,
  update      :: Tree s v -> Int -> v
}

explore :: (Monad m, Game a s) => Strategy s m v -> Tree s v -> m (Tree s v)

explore strategy leaf @ Leaf {state}
  | Game.terminal state = return $ terminal strategy leaf
  | otherwise = initNode strategy state

explore strategy node @ Node {state, value, children} = do
    index <- pick strategy node
    explored <- explore strategy (children IntMap.! index)
    let newChildren = IntMap.insert index explored children
    let node' = node {children = newChildren}
    let newValue = update strategy node' index
    return $ node' {value = newValue}

mcts :: (Monad m, Game a s) => Strategy s m v -> Int -> s -> m (Tree s v)
mcts strategy n =
  (initLeaf (heuristic strategy)) >=> (Utils.iterateM n $ explore strategy)  

