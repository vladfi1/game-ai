{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Pipes
import qualified Pipes.Prelude as P

import Data.Functor
import Control.Monad.Random
import qualified Data.Map as Map

import Control.Concurrent

import Threes
import NewGame
import ThreesGui


makePlayer player game @ PlayerState {} = player game
main = do
  guiHumanPlayer <-  makeGuiHumanPlayer
  
  setStdGen $ mkStdGen 0
  runEffect $  playOutM  (makePlayer guiHumanPlayer) newGame >-> P.drain
