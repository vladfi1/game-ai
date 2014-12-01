{-# LANGUAGE MultiParamTypeClasses, RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module NewGame where

import Pipes
import qualified Pipes.Prelude as P
--import Data.Monoid
import qualified Control.Monad.Random as Random

import Utils (maximumByKey, iterateN)
import Discrete
--import Instances

--data Nature
--data Player

type family Agent s
type family Action s

data GameState s =
  Nature {
    state :: s,
    nature :: (Fractional w, MonadDiscrete w m) => m (GameState s)
  } |
  Player {
    state :: s,
    agent :: Agent s,
    actions :: [(Action s, GameState s)]
  }

isPlayer Player {} = True
isPlayer _ = False

instance (Show s, Show (Agent s), Show (Action s)) => Show (GameState s) where
  show Player {state, agent, actions} =
    (show agent) ++ " to move:\n" ++ (show state) ++ "Legal moves: " ++ (show $ map fst actions)

terminal Player {actions = []} = True
terminal _ = False

type Heuristic s = GameState s -> Agent s -> Double

lookAheadEval :: forall s. Heuristic s -> Heuristic s
lookAheadEval heuristic Nature {nature} = \a ->
  expectation $ fmap (\state -> heuristic state a) (nature :: Discrete Double (GameState s))

lookAheadEval heuristic state @ Player {agent, actions}
  | terminal state = heuristic state
  | otherwise      = maximumByKey ($ agent) (fmap (heuristic . snd) actions)

lookAheadEvalDepth :: Int -> Heuristic s -> Heuristic s
lookAheadEvalDepth n = iterateN n lookAheadEval

lookAheadPlay :: Heuristic s -> GameState s -> GameState s
lookAheadPlay heuristic Player {agent, actions} =
  maximumByKey (\state -> heuristic state agent) (fmap snd actions)

lookAheadPlayDepth :: Int -> Heuristic s -> GameState s -> GameState s
lookAheadPlayDepth n heuristic = lookAheadPlay (lookAheadEvalDepth (n - 1) heuristic)

--playOut :: (GameState s -> GameState s) -> GameState s -> [GameState s]
--playOut play state@Terminal {} = [state]
--playOut play state = state : playOut play (play state)

--finalState :: Game a s => (s -> s) -> s -> s
--finalState = until terminal

--playOutEval :: Game a s => (s -> s) -> Heuristic a s
--playOutEval play state = evaluate (finalState play state)

playOutM :: (Monad m) => (GameState s -> m (GameState s)) -> GameState s -> Producer (GameState s) m ()
--playOutM play state@Terminal {} = return [state]
playOutM play state
  | terminal state = yield state
  | otherwise = do
      yield state
      next <- lift $ play state
      playOutM play next

{-
playOutEvalM :: (Monad m) => (GameState s -> m (GameState s)) -> GameState s -> m (a -> Double)
playOutEvalM play state = do
  game <- playOutM play state
  let final = last game
  return $ value final
--}

randomPlayer :: (Fractional w, MonadDiscrete w m) => GameState s -> m (GameState s)
randomPlayer Player {actions} = uniform (fmap snd actions)
randomPlayer Nature {nature} = nature

{-
playOutEvalR :: (MonadDiscrete w m) => GameState s -> m (a -> Double)
playOutEvalR = playOutEvalM randomPlayer

playOutEvalPR :: (GameState s -> Int) -> Heuristic a s
playOutEvalPR hash state = Random.evalRand (playOutEvalR state) (Random.mkStdGen $ hash state)
--}

naturePlayer Nature {nature} = Random.evalRandIO nature

