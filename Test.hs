import UCT
import Game
import Connect4
import qualified Data.IntMap as IntMap

n = 50
m = 1

strat = uct n (playOutEval (lookAheadPlayDepth m evaluate))
player = state . bestChild . strat

game = playOut player newGame
nodes = map strat game

inspect node = map (($ agent node) . value) (IntMap.elems $ children node)

main = putStrLn $ unlines $ map showBoard game
