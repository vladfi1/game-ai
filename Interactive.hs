module Interactive where

import Connect4 (Connect4Board, winner)
import qualified Connect4
import Game (Game, Heuristic, terminal, actions)
import qualified Game
import qualified UCT

import qualified Data.Map as Map
import Control.Monad.State

type Player s = s -> IO s

humanPlayer board = do
  print board
  print "Enter a number: "
  line <- getLine
  let index = read line
  return $ (actions board) !! index

n = 2000
m = 1

--heuristic = (Game.playOutEval (Game.lookAheadPlayDepth m Game.evaluate))
heuristic = (Game.lookAheadEvalDepth m Game.evaluate)

cpuPlayer board = do
  let node = UCT.bestChild $ UCT.uct n heuristic board
  print $ (UCT.getValue node) (Game.agent board)
  return $ UCT.state node

players :: Map.Map Connect4.Player (Player Connect4Board)
players = Map.fromList [(Connect4.O, humanPlayer), (Connect4.X, cpuPlayer)]

move :: StateT Connect4Board IO ()
move = do
  board <- get
  let player = players Map.! (Connect4.player board)
  next <- liftIO $ player board
  put next

playGame = do
  board <- get
  if terminal board
    then return ()
    else do move
            playGame

main = do
  (_, final) <- runStateT playGame Connect4.newGame
  print $ "The winner is " ++ (show $ winner final)
  print final
