module Main where

import qualified UCT
import UCT
import qualified Game
import Connect4 (newGame)
import qualified Utils
import Interactive (think)

n = 1000
m = 1

play state = 
  let node = think state in
  (state, node) : if Game.terminal state then []
    else play (UCT.state $ UCT.bestChild node)

game = play newGame
nodes = map snd game

inspect node = do
  print $ UCT.state node
  print $ map ((Utils.decimals 2) . ($ agent node) . winRatio) (listChildren node)
  print $ map getVisits (listChildren node)

main = sequence $ map inspect nodes

