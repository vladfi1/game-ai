{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module TrainNN where

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

type Datum = Vector Double
type Data = Matrix Double

type DataSet s = [(s, Datum)]

prepare :: (Convertible s Datum) => DataSet s -> (Data, Data)
prepare dataset = (inMat, outMat)
  where (input, output) = unzip dataset
        inMat = fromRows . (map convert) $ input
        outMat = fromRows output

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

trainNN :: (Convertible s Datum) =>
  TrainConfig -> DataSet s -> GenericModel -> (GenericModel, Double)
trainNN TrainConfig {algorithm, precision, iterations} dataset model =
  let (inMat, outMat) = prepare dataset
      trained = trainModel model algorithm precision iterations inMat outMat
      score = scoreNN trained dataset
  in (trained, score)

scoreNN :: (Convertible s Datum) => GenericModel -> DataSet s -> Double
scoreNN GenericModel {cost, net} = uncurry (getCostFunction cost $ net) . prepare

toHeuristic :: (Convertible s Datum, Bounded (Agent s), Enum (Agent s)) => GenericModel -> Heuristic' s
toHeuristic model = fromVector . (getOutput model) . convert

