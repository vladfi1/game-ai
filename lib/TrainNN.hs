{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverlappingInstances #-}

module TrainNN where

import Debug.Trace
import Data.IORef

import AI.HNN.FF.Network

import Data.Packed.Vector
import Data.Packed.Matrix

import Control.Monad
import Control.Monad.Random
import Control.Applicative ((<$>))

import Convertible
import NewGame
import Utils (fromVector)
import Runner

type Datum = Vector Double
type Data = Samples Double

--prepare :: (Convertible f Datum, Convertible v Datum) => DataSet f v -> Data
--prepare = map project
--  where project (f, v) = (convert f, convert v)

dims mat = (rows mat, cols mat)

data NNConfig s = NNConfig
  { inputs :: Int
  , layers :: [Int]
  , outputs :: Int
  , activation :: ActivationFunction Double
  , activation' :: ActivationFunctionDerivative Double
  , learningRate :: Double
  
  , inputDatum :: s -> Datum
  , outputDatum :: s -> Datum
  , interpret :: Datum -> Agent s -> Double
  , depth :: Int
  , callback :: s -> IO ()
  }

initNN :: NNConfig s -> IO (Network Double)
initNN NNConfig {inputs, layers, outputs} = createNetwork inputs layers outputs

trainNN :: NNConfig s -> Data -> Int -> Network Double -> Network Double
trainNN NNConfig {activation, activation', learningRate} dataset iterations network =
  trainNTimes iterations learningRate activation activation' network dataset

{-
scoreNN :: (Convertible f Datum, Convertible v Datum) =>
  Network -> DataSet f v -> Double
scoreNN Network {cost, net} = uncurry (getCostFunction cost $ net) . prepare
-}

getHeuristic NNConfig {activation, inputDatum, interpret} network game =
  interpret $ output network activation $ inputDatum $ state game

tdLearn :: NNConfig s -> IORef (Network Double) -> GameState s -> IO s

tdLearn config modelRef = go where
  go gameState =
    let current = state gameState
        input = inputDatum config current in
    if terminal gameState
      then do
            let true = outputDatum config current
            --print $ true
            
            before <- getOutput input
            --print $ before
            
            reinforce input true 1
            
            after <- getOutput input
            --print $ after
            
            return current
      else do
            heuristic <- getHeuristic config <$> (readIORef modelRef)
            next <- lookAheadPlayDepth' (depth config) heuristic gameState
            next' <- nextPlayer next
            future <- getOutput $ inputDatum config (state next')
            reinforce input future 1
            go next'
  
  reinforce input out iters = modifyIORef modelRef train
    where train = trainNN config dataset iters
          dataset = [(input, out)]
  
  getOutput input = do
    model <- readIORef modelRef
    return $ output model (activation config) input

getDataset config initial player = do
  (game, final) <- playOutM' player initial
  
  let states = final : (map fst game)
  let value = outputDatum config $ state final
  let dataset = [(inputDatum config $ state s, value) | s <- states]
  
  return dataset

batchLearn :: (Ord (Action s)) => NNConfig s -> IORef (Network Double) -> Int -> GameState s -> IO s

batchLearn config modelRef batch initial = do
  heuristic <- getHeuristic config <$> (readIORef modelRef)
  let player = lookAheadPlayDepth (depth config) heuristic
  
  datasets <- replicateM batch $ getDataset config initial player
  
  modifyIORef modelRef $ trainNN config (concat datasets) 1
  
  state . snd <$> playOutM' player initial

