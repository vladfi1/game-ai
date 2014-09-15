module Main where

import qualified UCT
import UCT
import qualified Game
import KJC (newGame)
import qualified Utils
import Interactive (think)

n = 10

play state = 
  let node = think n state in
  (state, node) : if Game.terminal state then []
    else play (UCT.state $ UCT.bestChild node)

game = play newGame
nodes = map snd game

inspect node = do
  print $ UCT.state node
  print $ map ((Utils.decimals 2) . ($ agent node) . winRatio) (listChildren node)
  print $ map getVisits (listChildren node)

main = sequence $ map inspect nodes

