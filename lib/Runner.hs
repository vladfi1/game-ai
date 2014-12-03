module Runner where

import Pipes
import qualified Pipes.Prelude as P

import Data.Serialize
import Data.ByteString (writeFile, readFile)

import Prelude hiding (writeFile, readFile)

import System.Directory
import System.Random

import Data.Traversable
import Data.Either

import NewGame

type Saved s = ([(s, Action s)], s)

recordGame dirName initial player = do
  (game, final) <- P.fold' (flip (:)) [] id (playOutM player initial)
  
  let game' = reverse [(state s, act) | (s, act) <- game]
  
  createDirectoryIfMissing True dirName
  
  hash <- randomIO :: IO Int
  let fileName = dirName ++ (show hash)
  writeFile fileName $ encode (game', state final)

readGame fileName = do
  putStrLn fileName
  bytes <- readFile fileName
  return $ decode bytes

readGames dirName = do
  files <- getDirectoryContents dirName
  let gameFiles = map (dirName ++) . (filter $ (flip notElem) [".", ".."]) $ files
  games <- forM gameFiles readGame
  return $ rights games

