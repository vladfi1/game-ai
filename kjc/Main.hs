import qualified Interactive
import Interactive (humanPlayer, cpuPlayer)
import KJC (newGame, Player(..))

players O = cpuPlayer
players X = humanPlayer

main = Interactive.main players newGame
