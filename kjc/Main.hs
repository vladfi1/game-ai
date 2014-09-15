import qualified Interactive
import Interactive (cpuPlayer)
import KJC (newGame, Player(..), humanPlayer)

players O = cpuPlayer
players X = humanPlayer

main = Interactive.main players newGame
