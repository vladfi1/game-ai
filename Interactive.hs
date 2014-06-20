module Interactive where

import Connect4 (Connect4Board, winner)
import qualified Connect4
import Game (Game, terminal, actions)
import qualified Game
import qualified UCT

import qualified Data.Map as Map
import Control.Monad.State

type Player s = s -> IO s

humanPlayer :: Player Connect4Board
humanPlayer board = do
  putStrLn $ show board
  putStrLn "Enter a number: "
  line <- getLine
  let index = read line
  return $ (actions board) !! index

n = 500
m = 2

heuristic = (Game.playOutEval (Game.lookAheadPlayDepth m Game.evaluate))

cpuPlayer :: Player Connect4Board
cpuPlayer = return . (UCT.uctPlayer n heuristic)

players :: Map.Map Connect4.Player (Player Connect4Board)
players = Map.fromList [(Connect4.X, humanPlayer), (Connect4.O, cpuPlayer)]

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
  putStrLn $ "The winner is " ++ (show $ winner final)
  putStrLn $ show final
