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

data GameState a k s =
  Nature {state :: s, nature :: (Fractional w, MonadDiscrete w m) => m (GameState a k s) } |
  Player {state :: s, agent :: a, actions :: [(k, GameState a k s)]}

instance (Show s, Show k, Show a) => Show (GameState a k s) where
  show Player {state, agent, actions} =
    (show agent) ++ " to move:\n" ++ (show state) ++ "Legal moves: " ++ (show $ map fst actions)

terminal Player {actions = []} = True
terminal _ = False

type Heuristic a k s = GameState a k s -> a -> Double

lookAheadEval :: forall a k s. Heuristic a k s -> Heuristic a k s
lookAheadEval heuristic Nature {nature} = \a ->
  expectation $ fmap (\state -> (heuristic state a)) (nature :: Discrete Double (GameState a k s))

lookAheadEval heuristic state @ Player {agent, actions}
  | terminal state = heuristic state
  | otherwise      = maximumByKey ($ agent) (fmap (heuristic . snd) actions)

lookAheadEvalDepth :: Int -> Heuristic a k s -> Heuristic a k s
lookAheadEvalDepth n = iterateN n lookAheadEval

lookAheadPlay :: Heuristic a k s -> GameState a k s -> GameState a k s
lookAheadPlay heuristic Player {agent, actions} =
  maximumByKey (\state -> heuristic state agent) (fmap snd actions)

lookAheadPlayDepth :: Int -> Heuristic a k s -> GameState a k s -> GameState a k s
lookAheadPlayDepth n heuristic = lookAheadPlay (lookAheadEvalDepth (n - 1) heuristic)

--playOut :: (GameState a k s -> GameState a k s) -> GameState a k s -> [GameState a k s]
--playOut play state@Terminal {} = [state]
--playOut play state = state : playOut play (play state)

--finalState :: Game a s => (s -> s) -> s -> s
--finalState = until terminal

--playOutEval :: Game a s => (s -> s) -> Heuristic a s
--playOutEval play state = evaluate (finalState play state)

playOutM :: (Monad m) => (GameState a k s -> m (GameState a k s)) -> GameState a k s -> m [GameState a k s]
--playOutM play state@Terminal {} = return [state]
playOutM play state
  | terminal state = return [state]
  | otherwise = do
      next <- play state
      rest <- playOutM play next
      return $ state : rest
{-
playOutEvalM :: (Monad m) => (GameState a k s -> m (GameState a k s)) -> GameState a k s -> m (a -> Double)
playOutEvalM play state = do
  game <- playOutM play state
  let final = last game
  return $ value final
--}

randomPlayer :: (Fractional w, MonadDiscrete w m) => GameState a k s -> m (GameState a k s)
randomPlayer Player {actions} = uniform (fmap snd actions)
randomPlayer Nature {nature} = nature

{-
playOutEvalR :: (MonadDiscrete w m) => GameState a k s -> m (a -> Double)
playOutEvalR = playOutEvalM randomPlayer

playOutEvalPR :: (GameState a k s -> Int) -> Heuristic a s
playOutEvalPR hash state = Random.evalRand (playOutEvalR state) (Random.mkStdGen $ hash state)
--}
