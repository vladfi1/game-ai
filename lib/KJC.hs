{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module KJC (module KJC, module TwoPlayer) where

import TwoPlayer

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Vector (Vector)
import qualified Data.Vector

import Data.Maybe (isJust)

import Debug.Trace

import Grid (Square)
import qualified Grid
import qualified Utils
import Game (Game, agent, terminal, actions, evaluate)
import TwoPlayer

import System.Console.ANSI

width = 6
height = 6
dims = (width, height)
xrange = Utils.range width
yrange = Utils.range height

squares = Grid.squares dims
numSquares = length squares
grid = Grid.grid dims
neighbors = Utils.fromMap grid
degrees = Utils.fromMap (Map.map length grid)

data KJCState = KJCState {
--  total :: Int,
  player :: Player,
  values :: Map Square Int,
  owners :: Map Square Player,
--  owned  :: Vector (Set Square),
  owned  :: Player -> (Set Square),
  winner :: Maybe Player
}

newGame = KJCState {
--  total = 0,
  player = X,
  values = Map.fromList [(square, 0) | square <- squares],
  owners = Map.empty,
--  owned  = Utils.toVector (const Set.empty),
  owned  = const Set.empty,
  winner = Nothing
}

assign player square owned = Utils.mem (\p ->
  (if p == player then Set.insert else Set.delete) square (owned p))

check state @ KJCState {player, owned} state' =
  if Set.size (owned player) == numSquares
    then state {winner = Just player}
    else state'

increment :: KJCState -> [Square] -> KJCState

increment state @ KJCState {player} [] =
  check state $
  state {player = other player}

increment state @ KJCState {player, values, owners, owned} (next:rest) =
  check state $
  let value = values Map.! next + 1
      state' = state {
        owners = Map.insert next player owners,
        owned = assign player next owned
      }
  in if value == degrees next
    then increment state' {
      values = Map.insert next 0 values
    } (neighbors next ++ rest)
    else increment state' {
      values = Map.insert next value values
    } rest

play state square = increment state [square]

playable KJCState {player, owned} =
  filter (\square -> Set.notMember square banned) squares
    where banned = owned (other player)

instance Game Player KJCState where
  agent = player
  terminal = isJust . winner
  actions state = map (play state) (playable state)
  evaluate state = fromIntegral . Set.size . (owned state)

instance Show KJCState where
  show KJCState {player, values, owners} =
    unlines [(concat [toString (x, y) | x <- xrange]) ++ reset | y <- reverse yrange]
    ++ "Player " ++ (show $ player) ++ " to move."
    where reset = setSGRCode []
          toString square = (colorCode square) ++ (show $ values Map.! square)
          colorCode square = setSGRCode [SetColor Background Vivid (getColor $ Map.lookup square owners)]
          getColor (Just X) = Red
          getColor (Just O) = Blue
          getColor Nothing = White

readSquare legal = do
  putStrLn "Enter a pair:"
  line <- getLine
  let [x, y] = map read (words line)
  let square = (x, y)
  if Set.member square legal
    then return square
    else readSquare legal 

humanPlayer :: KJCState -> IO KJCState
humanPlayer state = do
  --print state
  square <- readSquare $ Set.fromList (playable state)
  return $ play state square
