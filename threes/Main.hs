{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Functor
import Control.Monad
import Control.Monad.Random
import qualified Data.Map as Map

import Discrete (choose)
import Threes
import NewGame
import Runner
import Types

humanPlayer game @ PlayerState {actions} = do
  print game
  line <- getLine
  let tile = read line :: Direction
  return $ (Map.fromList actions) Map.! tile

cpuPlayer = (lookAheadPlayDepth 4 basicHeuristic)

saveDir = "saved/threes/"

testRunner = do
  recordGame saveDir newGame cpuPlayer
  
  games <- readDir saveDir
  
  let (game :: Saved ThreesState) = (games !! 0)
  
  --forM game print
  print game

unaryOutput = (scoreThreesUnary, numTileClasses)
binaryOutput = (scoreThreesBits, 4)

(scoreFunction, outputLayer) = unaryOutput


main = do
  setStdGen $ mkStdGen 0
  recordGame saveDir newGame cpuPlayer
  --putStrLn "Playing games"
  --replicateM 500 playGame
