module Interactive where

import qualified Game
import Game (Game, Heuristic)
import qualified Game
import qualified UCT
import Utils (inRange, decimals)

import Control.Monad.State
import Control.Monad.Random

pick max = do
  print $ "Enter an integer in [0, " ++ (show max) ++ ")"
  line <- getLine
  let input = read line :: Int
  if inRange (0, max) input
    then return input
    else pick max

humanPlayer board = do
  print board
  let acts = Game.actions board
  index <- pick (length acts)
  return $ acts !! index

n = 2000

think board = evalRand (UCT.uct n Game.playOutEvalR board) (mkStdGen 0)

cpuPlayer board = do
  let node = think board
  print $ map ((decimals 2) . ($ UCT.agent node) . UCT.winRatio) (UCT.listChildren node)
  print $ map UCT.getVisits (UCT.listChildren node)
  return $ UCT.state $ UCT.bestChild node

playGame players = let
  move = do
    board <- get
    let player = players (Game.agent board)
    next <- liftIO $ player board
    put next
  playGame' = do
    board <- get
    if Game.terminal board
      then return ()
      else do move
              playGame'
  in playGame'

main players initial = do
  (_, final) <- runStateT (playGame players) initial
--  print $ "The winner is " ++ (show $ winner final)
  print final
