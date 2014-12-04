{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverlappingInstances #-}

module TrainNN where

import Debug.Trace


import AI.Model
import AI.Training
import AI.Calculation

import Data.Packed.Vector
import Data.Packed.Matrix

import Control.Monad.Random
import Control.Applicative ((<$>))

--import Convertible
import NewGame
import Utils (fromVector)

type Datum = Vector Double
type Data = Matrix Double

type DataSet = [(Datum, Datum)]

prepare :: DataSet -> (Data, Data)
prepare dataset = (inMat, outMat)
  where (input, output) = unzip dataset
        inMat = fromRows input
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

dims mat = (rows mat, cols mat)

trainNN :: TrainConfig -> DataSet -> GenericModel -> (GenericModel, Double)
trainNN TrainConfig {algorithm, precision, iterations} dataset model =
  let (inMat, outMat) = prepare dataset
      trained = trainModel model algorithm precision iterations inMat outMat
      score = scoreNN trained dataset
  in traceShow (dims inMat, dims outMat) (trained, score)

scoreNN :: GenericModel -> DataSet -> Double
scoreNN GenericModel {cost, net} = uncurry (getCostFunction cost $ net) . prepare

toHeuristic :: (Bounded (Agent s), Enum (Agent s)) =>
  GenericModel -> (s -> Datum) -> Heuristic' s
toHeuristic model toDatum = fromVector . (getOutput model) . toDatum

