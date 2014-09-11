import qualified Interactive
import Interactive (humanPlayer, cpuPlayer)
import Connect4 (newGame, Player(..))

players X = cpuPlayer
players O = humanPlayer

main = Interactive.main players newGame
