{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Game where

import Utils (maximumByKey, iterateN)

type Heuristic a s = s -> a -> Double

class Game a s | s -> a where
  agent :: s -> a
  actions :: s -> [s]
  terminal :: s -> Bool
  evaluate :: Heuristic a s

lookAheadEval :: Game a s => Heuristic a s -> Heuristic a s
lookAheadEval heuristic state =
  if terminal state then heuristic state else
    maximumByKey ($ (agent state)) (map heuristic (actions state))

lookAheadEvalDepth :: Game a s => Int -> Heuristic a s -> Heuristic a s
lookAheadEvalDepth n = iterateN n lookAheadEval

lookAheadPlay :: Game a s => Heuristic a s -> s -> s
lookAheadPlay heuristic state =
  maximumByKey ((flip heuristic) (agent state)) (actions state)

lookAheadPlayDepth :: Game a s => Int -> Heuristic a s -> s -> s
lookAheadPlayDepth n heuristic = lookAheadPlay (lookAheadEvalDepth (n - 1) heuristic)

playOut :: Game a s => (s -> s) -> s -> [s]
playOut play state =
  if terminal state then [state]
    else state : playOut play (play state)

finalState :: Game a s => (s -> s) -> s -> s
finalState = until terminal

playOutEval :: Game a s => (s -> s) -> Heuristic a s
playOutEval play state = evaluate (finalState play state)

