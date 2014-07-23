module Test where

import qualified UCT
import UCT (uct, agent, getValue, children, visits)
import qualified Game
import Connect4 (newGame)
import qualified Data.IntMap as IntMap
import qualified Utils

n = 2000
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
  print $ map ((Utils.decimals 2) . ($ agent node) . getValue) (IntMap.elems $ children node)
  print $ map visits (IntMap.elems $ children node)

main = sequence $ map inspect nodes

