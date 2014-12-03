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

--import qualified Control.Monad.Random as Random

import qualified Data.Map as Map

import Utils (maximumByKey, iterateN)
import Discrete

type family Agent s
type family Action s

data GameState s =
  NatureState {
    state :: s,
    nature :: (Fractional w, MonadDiscrete w m) => m (GameState s)
  } |
  PlayerState {
    state :: s,
    agent :: Agent s,
    actions :: [(Action s, GameState s)]
  }

isPlayerState PlayerState {} = True
isPlayerState _ = False

instance (Show s, Show (Agent s), Show (Action s)) => Show (GameState s) where
  show PlayerState {state, agent, actions} =
    (show agent) ++ " to move:\n" ++ (show state) ++ "Legal moves: " ++ (show $ map fst actions)

terminal PlayerState {actions = []} = True
terminal _ = False

type Heuristic s = GameState s -> Agent s -> Double
type Player s m = GameState s -> m (Action s)

lookAheadEval :: forall s m. Heuristic s -> Heuristic s
lookAheadEval heuristic NatureState {nature} = \a ->
  expectation $ fmap (\state -> heuristic state a) (nature :: Discrete Double (GameState s))

lookAheadEval heuristic state @ PlayerState {agent, actions}
  | terminal state = heuristic state
  | otherwise      = maximumByKey ($ agent) (fmap (heuristic . snd) actions)

lookAheadEvalDepth :: Int -> Heuristic s -> Heuristic s
lookAheadEvalDepth n = iterateN n lookAheadEval

lookAheadPlay :: (Monad m) => Heuristic s -> Player s m
lookAheadPlay heuristic PlayerState {agent, actions} =
  return . fst $ maximumByKey (\(_, state) -> heuristic state agent) actions

lookAheadPlayDepth :: (Monad m) => Int -> Heuristic s -> Player s m
lookAheadPlayDepth n heuristic = lookAheadPlay (lookAheadEvalDepth (n - 1) heuristic)

--playOut :: (GameState s -> GameState s) -> GameState s -> [GameState s]
--playOut play state@Terminal {} = [state]
--playOut play state = state : playOut play (play state)

--finalState :: Game a s => (s -> s) -> s -> s
--finalState = until terminal

--playOutEval :: Game a s => (s -> s) -> Heuristic a s
--playOutEval play state = evaluate (finalState play state)

playOutM :: (Ord (Action s), Fractional w, MonadDiscrete w m) => Player s m -> GameState s -> Producer (GameState s, Action s) m (GameState s)

playOutM play state @ NatureState {nature} = (lift nature) >>= (playOutM play)

playOutM play state @ PlayerState {actions}
  | terminal state = return state
  | otherwise = do
      act <- lift $ play state
      yield (state, act)
      let next = (Map.fromList actions) Map.! act
      playOutM play next

{-
playOutEvalM :: (Monad m) => (GameState s -> m (GameState s)) -> GameState s -> m (a -> Double)
playOutEvalM play state = do
  game <- playOutM play state
  let final = last game
  return $ value final
--}

randomPlayer :: (Fractional w, MonadDiscrete w m) => Player s m
randomPlayer PlayerState {actions} = uniform (fmap fst actions)
--randomPlayer NatureState {nature} = nature

{-
playOutEvalR :: (MonadDiscrete w m) => GameState s -> m (a -> Double)
playOutEvalR = playOutEvalM randomPlayerState

playOutEvalPR :: (GameState s -> Int) -> Heuristic a s
playOutEvalPR hash state = Random.evalRand (playOutEvalR state) (Random.mkStdGen $ hash state)
--}


