module UCT (module UCT, module MCTS) where

import Game (Game, Heuristic)
import qualified Game

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Data.Maybe (catMaybes)

import Data.Monoid (Monoid, mempty, mappend, (<>), mconcat)

import Utils (listToIntMap, maximumByKey, iterateN, toVector, fromVector)

import MCTS

data Stats a = Stats {
  visits :: Double,
  wins :: Vector Double
}

getVisits = visits . MCTS.value

instance (Bounded a, Enum a) => Monoid (Stats a) where
  mempty = toStats (const 0)
  mappend s1 s2 = Stats {
    visits = visits s1 + visits s2,
    wins = Vector.zipWith (+) (wins s1) (wins s2)
  }

toStats :: (Bounded a, Enum a) => (a -> Double) -> Stats a
toStats heuristic =
  let wins = toVector heuristic in
    Stats {visits = Vector.sum wins, wins = wins}

winRatio node = let stats = value node in
  \a -> (fromVector $ wins stats) a / (visits stats)

bestChild node = maximumByKey (\child -> (winRatio child) (agent node)) (listChildren node)

ucb node = maximumByKey
  (\(_, child) -> sqrt (2 * log (getVisits node) / getVisits child) + winRatio child (agent node))
  (IntMap.assocs $ children node)

increment node = node {
  value = (value node) <> (toStats $ winRatio node)
}

sumChildren node =
  mconcat $ map value (listChildren node)

strategy :: (Game a s, Bounded a, Enum a) => Strategy s (Stats a)
strategy = Strategy {
  pick = fst . ucb,
  terminal = increment,
  incorporate = \node _ -> sumChildren node
}

--uct :: (Game a s) => Int -> Heuristic a s -> s -> Node a s
uct n heuristic = mcts strategy n (toStats . heuristic)
