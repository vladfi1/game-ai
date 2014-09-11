{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Game where

import Control.Monad.Random

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

playOutM :: (Monad m, Game a s) => (s -> m s) -> s -> m [s]
playOutM play state =
  if terminal state then return [state]
    else do
      next <- play state
      rest <- playOutM play next
      return $ state : rest

playOutEvalM :: (Monad m, Game a s) => (s -> m s) -> s -> m (a -> Double)
playOutEvalM play state = do
  game <- playOutM play state
  let final = last game
  return $ evaluate final

randomPlayer :: (MonadRandom m, Game a s) => s -> m s
randomPlayer state = do
  let states = actions state
  index <- getRandomR (0, length states + 1)
  return $ states !! index

playOutEvalR :: (MonadRandom m, Game a s) => s -> m (a -> Double)
playOutEvalR = playOutEvalM randomPlayer

