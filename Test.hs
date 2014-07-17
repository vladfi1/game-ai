module Test where

import UCT
import qualified Game
import Connect4 (newGame)
import qualified Data.IntMap as IntMap

n = 2000
m = 1

--strat = uct n (Game.playOutEval (Game.lookAheadPlayDepth m Game.evaluate))
strat = uct n (Game.lookAheadEvalDepth m Game.evaluate)
play = state . bestChild . strat

game = Game.playOut play newGame
nodes = map strat game

inspect node = do
  print $ state $ node
  print $ map (($ agent node) . value) (IntMap.elems $ children node)
  print $ map visits (IntMap.elems $ children node)

main = sequence $ map inspect nodes

