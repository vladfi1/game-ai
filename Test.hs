module Test where

import UCT
import qualified Game
import Connect4 (newGame)
import qualified Data.IntMap as IntMap

main = putStrLn $ unlines $ map show game

n = 2000
m = 1

--strat = uct n (Game.playOutEval (Game.lookAheadPlayDepth m Game.evaluate))
strat = uct n (Game.lookAheadEvalDepth m Game.evaluate)
play = state . bestChild . strat

game = Game.playOut play newGame
nodes = map strat game

inspect node = map (($ agent node) . value) (IntMap.elems $ children node)

