import UCT
import Game
import TicTacToe

uctPlayer n m = uct n (playOutEval (lookAheadPlayDepth m evaluate))

main = putStrLn $ unlines $ map showBoard (playOut (uctPlayer 50 1) newGame)
