{-# LANGUAGE NoMonomorphismRestriction #-}

module Interactive where

import qualified Game
import Game (Game)
import qualified Game
import qualified UCT
import Utils (inRange)

import qualified Data.Map as Map
import Control.Monad.State

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
m = 1

--heuristic = (Game.playOutEval (Game.lookAheadPlayDepth m Game.evaluate))
heuristic = (Game.lookAheadEvalDepth m Game.evaluate)

cpuPlayer board = do
  let node = UCT.bestChild $ UCT.uct n heuristic board
  print $ (UCT.getValue node) (Game.agent board)
  return $ UCT.state node

playGame players = let
  move = do
    board <- get
    let player = players Map.! (Game.agent board)
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
