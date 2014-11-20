{-# LANGUAGE MultiParamTypeClasses, RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NewGame where

--import Data.Monoid
import qualified Control.Monad.Random as Random

import Utils (maximumByKey, iterateN)
import Discrete
--import Instances

--data Nature
--data Player

data GameState a s =
  Nature {state :: s, nature :: (MonadDiscrete w m) => m (GameState a s) } |
  Player {state :: s, agent :: a, actions :: [GameState a s]} |
  Terminal {state :: s, value :: a -> Double}

type Heuristic a s = GameState a s -> a -> Double

lookAheadEval :: forall a s. Heuristic a s -> Heuristic a s
lookAheadEval heuristic Terminal {value} = value

lookAheadEval heuristic Nature {nature} = \a ->
  expectation $ fmap (\state -> (heuristic state a)) (nature :: Discrete Double (GameState a s))

lookAheadEval heuristic Player {agent, actions} =
  maximumByKey ($ agent) (fmap heuristic actions)


lookAheadEvalDepth :: Int -> Heuristic a s -> Heuristic a s
lookAheadEvalDepth n = iterateN n lookAheadEval

lookAheadPlay :: Heuristic a s -> GameState a s -> GameState a s
lookAheadPlay heuristic Player {agent, actions} =
  maximumByKey ((flip heuristic) agent) actions

lookAheadPlayDepth :: Int -> Heuristic a s -> GameState a s -> GameState a s
lookAheadPlayDepth n heuristic = lookAheadPlay (lookAheadEvalDepth (n - 1) heuristic)

--playOut :: (GameState a s -> GameState a s) -> GameState a s -> [GameState a s]
--playOut play state@Terminal {} = [state]
--playOut play state = state : playOut play (play state)

--finalState :: Game a s => (s -> s) -> s -> s
--finalState = until terminal

--playOutEval :: Game a s => (s -> s) -> Heuristic a s
--playOutEval play state = evaluate (finalState play state)

playOutM :: (Monad m) => (GameState a s -> m (GameState a s)) -> GameState a s -> m [GameState a s]
playOutM play state@Terminal {} = return [state]
playOutM play state = do
      next <- play state
      rest <- playOutM play next
      return $ state : rest

playOutEvalM :: (Monad m) => (GameState a s -> m (GameState a s)) -> GameState a s -> m (a -> Double)
playOutEvalM play state = do
  game <- playOutM play state
  let final = last game
  return $ value final

randomPlayer :: (MonadDiscrete w m) => GameState a s -> m (GameState a s)
randomPlayer Player {actions} = uniform actions
randomPlayer Nature {nature} = nature

playOutEvalR :: (MonadDiscrete w m) => GameState a s -> m (a -> Double)
playOutEvalR = playOutEvalM randomPlayer

playOutEvalPR :: (GameState a s -> Int) -> Heuristic a s
playOutEvalPR hash state = Random.evalRand (playOutEvalR state) (Random.mkStdGen $ hash state)

