{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverlappingInstances #-}

module TrainNN where

import Debug.Trace
import Data.IORef

import AI.Model
import AI.Training
import AI.Calculation

import Data.Packed.Vector
import Data.Packed.Matrix

import Control.Monad.Random
import Control.Applicative ((<$>))

import Convertible
import NewGame
import Utils (fromVector)
import Runner

type Datum = Vector Double
type Data = Matrix Double

prepare :: (Convertible f Datum, Convertible v Datum) => DataSet f v -> (Data, Data)
prepare dataset = (inMat, outMat)
  where (input, output) = unzip dataset
        inMat = fromRows $ (map convert) input
        outMat = fromRows $ (map convert) output

data ModelConfig = ModelConfig
  { activation      :: Activation
  , costModel       :: Cost
  , layers          :: [Int]
  , regularization  :: Double
  }

data TrainConfig = TrainConfig
  { algorithm :: TrainingAlgorithm
  , precision :: Double
  , iterations :: Int
  }

initNN :: (Functor m, MonadSplit StdGen m) => ModelConfig -> m GenericModel
initNN ModelConfig {activation, costModel, layers, regularization} =
  initializeModel activation costModel layers regularization <$> getSplit

dims mat = (rows mat, cols mat)

trainNN :: (Convertible f Datum, Convertible v Datum) =>
  TrainConfig -> DataSet f v -> GenericModel -> GenericModel
trainNN TrainConfig {algorithm, precision, iterations} dataset model = trained
  where (inMat, outMat) = prepare dataset
        trained = trainModel model algorithm precision iterations inMat outMat

scoreNN :: (Convertible f Datum, Convertible v Datum) =>
  GenericModel -> DataSet f v -> Double
scoreNN GenericModel {cost, net} = uncurry (getCostFunction cost $ net) . prepare

data TDConfig s = TDConfig
  { inputDatum :: s -> Datum
  , outputDatum :: s -> Datum
  , interpret :: Datum -> Agent s -> Double
  , depth :: Int
  , callback :: s -> IO ()
  }

getHeuristic TDConfig {inputDatum, interpret} model PlayerState {state} =
  interpret $ getOutput model $ inputDatum state

tdLearn :: TDConfig s -> (IORef GenericModel) -> (GameState s) -> IO ()

tdLearn config modelRef = go where
  go gameState =
    let s1 = state gameState
        input = inputDatum config s1 in
    if terminal gameState
      then do
            let output = outputDatum config s1
            reinforce input output
            callback config s1
      else do
            model <- readIORef modelRef
            let heuristic = getHeuristic config model
            next <- lookAheadPlayDepth' (depth config) heuristic gameState
            next' <- nextPlayer next
            let output = getOutput model $ inputDatum config (state next')
            reinforce input output
            go next'
  
  reinforce input output = modifyIORef modelRef train
    where train model = trainModel model GradientDescent 1.0 1 inMat outMat
          inMat = asRow input
          outMat = asRow output




