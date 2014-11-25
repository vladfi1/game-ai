{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad.Random
import qualified Data.Map as Map

import Threes
import NewGame

player game @ Player {actions} = do
  print game
  line <- getLine
  let tile = read line :: Direction
  return $ (Map.fromList actions) Map.! tile

player Nature {nature} = do
  stdGen <- getStdGen
  return $ evalRand nature stdGen

main = playOutM player newGame
