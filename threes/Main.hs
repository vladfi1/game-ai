{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Functor
import Control.Monad
import Control.Monad.Random
import qualified Data.Map as Map

import Threes
import NewGame


humanPlayer game @ Player {actions} = do
  print game
  line <- getLine
  let tile = read line :: Direction
  return $ (Map.fromList actions) Map.! tile

naturePlayer Nature {nature} = evalRandIO nature

cpuPlayer = return . (lookAheadPlayDepth 6 basicHeuristic)

makePlayer player game @ Player {} = player game
makePlayer player game @ Nature {} = naturePlayer game

main = playerStates >>= (flip forM $ print)
  where states = playOutM (makePlayer cpuPlayer) newGame
        playerStates = (filter isPlayer) <$> states
        

