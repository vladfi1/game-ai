module OnePlayer where

import Utils (allValues)

data Player = You
  deriving (Show, Eq, Ord, Enum, Bounded)

allPlayers :: [Player]
allPlayers = allValues

