import qualified Interactive
import Interactive (humanPlayer, cpuPlayer)
import Connect4 (newGame, Player(..))

players O = cpuPlayer
players X = humanPlayer

main = Interactive.main players newGame
