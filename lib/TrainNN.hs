{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
--{-# LANGUAGE ImpredicativeTypes #-}

module TrainNN where

import AI.Model
import AI.Training
import AI.Calculation

import Data.Packed.Vector
import Data.Packed.Matrix

import Control.Monad.Random
import Control.Applicative ((<$>))

import Convertible

type Datum = Vector Double

type DataSet s = [(s, Datum)]

--prepare :: DataSet s -> (Matrix Double, Matrix Double)


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
  TrainConfig -> DataSet s -> GenericModel -> GenericModel
trainNN TrainConfig {algorithm, precision, iterations} dataset model =
  let (input, output) = unzip dataset
      inMat = fromRows . (map convert) $ input
      outMat = fromRows output
  in trainModel model algorithm precision iterations inMat outMat

