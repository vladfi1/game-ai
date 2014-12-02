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

makePlayer player game @ Player {} = player game
makePlayer player game @ Nature {} = naturePlayer game

recordGame dirName initial player = do
  game <- P.toListM $ (playOutM (makePlayer player) initial) >-> (P.filter isPlayer) >-> (P.map state)
  
  createDirectoryIfMissing True dirName
  
  hash <- randomIO :: IO Int
  let fileName = dirName ++ (show hash)
  writeFile fileName (encode game)

readGame fileName = do
  putStrLn fileName
  bytes <- readFile fileName
  return $ decode bytes

readGames dirName = do
  files <- getDirectoryContents dirName
  let gameFiles = map (dirName ++) . (filter (\s -> (head s) /= '.')) $ files
  games <- forM gameFiles readGame
  return $ rights games

