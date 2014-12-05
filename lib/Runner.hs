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
import Convertible
import Utils (toVector)

import Data.Packed.Matrix
import Types


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

--savedToData :: (Datum s, Bounded (Agent s), Enum (Agent s)) => Heuristic' s -> Saved s -> DataSet s
savedToData :: forall s v. (Convertible s Datum, Bounded (Agent s), Enum (Agent s), Convertible v Datum) =>
  (s -> Agent s -> v) -> (s -> [s]) -> Saved s -> DataSet
savedToData score symmetries (game, final) = [(convert s, value) | s <- states]
  where value = foldMap convert ((toVector $ score final) :: Vector v)
        states = (final : (map fst game)) >>= symmetries

loadData dirName score symmetries = do
  putStrLn $ "Loading data from " ++ dirName
  games <- readDir dirName
  return $ games >>= (savedToData score symmetries)

