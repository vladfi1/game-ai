import qualified Interactive
import Interactive (humanPlayer, cpuPlayer)
import Connect4 (newGame, Player(..))

import qualified Data.Map as Map

players = Map.fromList [(O, humanPlayer), (X, cpuPlayer)]

main = Interactive.main players newGame
