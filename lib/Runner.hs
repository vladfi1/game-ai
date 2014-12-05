{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Runner where

import Pipes
import qualified Pipes.Prelude as P

import Data.Serialize
import qualified Data.ByteString as ByteString

import System.Directory
import System.Random

import Data.Foldable (foldMap)
--import Data.Traversable
import Data.Either
import Control.Monad

import Data.Vector (Vector)

import NewGame
import Convertible
import Utils (toVector)

import Data.Packed.Matrix

type Saved s = ([(s, Action s)], s)
type DataSet f v = [(f, v)]

recordGame dirName initial player = do
  (game, final) <- P.fold' (flip (:)) [] id (playOutM player initial)
  
  let game' = reverse [(state s, act) | (s, act) <- game]
  
  createDirectoryIfMissing True dirName
  
  hash <- randomIO :: IO Int
  let fileName = dirName ++ (show hash)
  ByteString.writeFile fileName $ encode (game', state final)

readGame fileName = do
  --putStrLn fileName
  bytes <- ByteString.readFile fileName
  return $ decode bytes

--readGames :: String -> IO (Saved s)
readDir dirName = do
  files <- getDirectoryContents dirName
  let gameFiles = map (dirName ++) . (filter $ (flip notElem) [".", ".."]) $ files
  games <- forM gameFiles readGame
  return $ rights games

savedToData :: (s -> [s]) -> Saved s -> DataSet s s
savedToData symmetries (game, final) = [(s, final) | s <- states]
  where states = (final : (map fst game)) >>= symmetries

data Trainer s f v t = Trainer
  { symmetries :: s -> [s]
  , projectIn :: s -> f
  , projectOut :: s -> v
  , train :: DataSet f v -> IO t
  , apply :: t -> Heuristic' s
  , name :: String
  }

loadData dirName Trainer {symmetries, projectIn, projectOut} = do
  putStrLn $ "Loading data from " ++ dirName
  games <- readDir dirName
  let dataset =  games >>= (savedToData symmetries)
  return $ map (\(input, output) -> (projectIn input, projectOut output)) dataset

iterate trainData trainer @ Trainer {train, apply, name} depth initial samples = do
  dataset <- loadData trainData trainer
  trained <- train dataset
  
  let heuristic = toHeuristic $ apply trained
  let player = lookAheadPlayDepth depth heuristic
  
  let newName = trainData ++ "-" ++ name ++ (show depth)
  
  let record = recordGame newName initial player
  
  replicateM samples $ record

