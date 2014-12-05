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

import Convertible
import NewGame
import Utils (fromVector)

type Datum = Vector Double
type Data = Matrix Double

type DataSet f v = [(f, v)]

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

toHeuristic :: (Bounded (Agent s), Enum (Agent s)) =>
  GenericModel -> (s -> Datum) -> Heuristic' s
toHeuristic model toDatum = fromVector . (getOutput model) . toDatum

