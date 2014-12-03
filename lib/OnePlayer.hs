module OnePlayer where

import Utils (allValues)

data OnePlayer = You
  deriving (Show, Eq, Ord, Enum, Bounded)

allPlayers :: [OnePlayer]
allPlayers = allValues

