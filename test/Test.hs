module Main where

import qualified UCT
import UCT
import qualified Game
import Connect4 (newGame)
import qualified Utils

n = 1000
m = 1

--strat = uct n (Game.playOutEval (Game.lookAheadPlayDepth m Game.evaluate))
strat = uct n (Game.lookAheadEvalDepth m Game.evaluate)
play state = 
  let node = strat state in
  (state, node) : if Game.terminal state then []
    else play (UCT.state $ UCT.bestChild node)

game = play newGame
nodes = map snd game

inspect node = do
  print $ UCT.state node
  print $ map ((Utils.decimals 2) . ($ agent node) . winRatio) (listChildren node)
  print $ map getVisits (listChildren node)

main = sequence $ map inspect nodes

