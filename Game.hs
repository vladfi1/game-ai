{-# LANGUAGE MultiParamTypeClasses #-}

module Game where

import Utils (maximumByKey, iterateN)

type Heuristic a b = (a, b) -> a -> Double

class Game a b where
  actions :: (a, b) -> [(a, b)]
-- play :: (a, b) -> c -> (a, b)
  terminal :: (a, b) -> Bool
  evaluate :: Heuristic a b

lookAheadEval :: Game a b => Heuristic a b -> Heuristic a b
lookAheadEval heuristic state =
  if terminal state then heuristic state else
    maximumByKey ($ (fst state)) (map heuristic (actions state))

lookAheadEvalDepth :: Game a b => Int -> Heuristic a b -> Heuristic a b
lookAheadEvalDepth n = iterateN n lookAheadEval

lookAheadPlay :: Game a b => Heuristic a b -> (a, b) -> (a, b)
lookAheadPlay heuristic (agent, state) =
  maximumByKey ((flip heuristic) agent) (actions (agent, state))

lookAheadPlayDepth :: Game a b => Int -> Heuristic a b -> (a, b) -> (a, b)
lookAheadPlayDepth n heuristic = lookAheadPlay (lookAheadEvalDepth (n - 1) heuristic)

playOut :: Game a b => ((a, b) -> (a, b)) -> (a, b) -> [(a, b)]
playOut play state =
  if terminal state then [state]
    else state : playOut play (play state)

finalState :: Game a b => ((a, b) -> (a, b)) -> (a, b) -> (a, b)
finalState = until terminal

playOutEval :: Game a b => ((a, b) -> (a, b)) -> Heuristic a b
playOutEval play state = evaluate (finalState play state)


