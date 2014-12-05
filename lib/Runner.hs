{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Runner where

import Pipes
import qualified Pipes.Prelude as P

import Data.Serialize
import qualified Data.ByteString as ByteString

import System.Directory
import System.Random

import Data.Foldable (foldMap)
import Data.Traversable
import Data.Either

import Data.Vector (Vector)

import NewGame
import TrainNN
import Convertible
import Utils (toVector)

type Saved s = ([(s, Action s)], s)

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

loadData dirName symmetries = do
  putStrLn $ "Loading data from " ++ dirName
  games <- readDir dirName
  return $ games >>= (savedToData symmetries)

