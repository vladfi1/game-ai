import UCT
import qualified Game
import Connect4
import qualified Data.IntMap as IntMap

n = 2000
m = 1

strat = uct n (Game.playOutEval (Game.lookAheadPlayDepth m Game.evaluate))
player = state . bestChild . strat

game = Game.playOut player newGame
nodes = map strat game

inspect node = map (($ agent node) . value) (IntMap.elems $ children node)

main = putStrLn $ unlines $ map showBoard game
