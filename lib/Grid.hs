module Grid where

import Prelude hiding ((+), (-), (*), sum, negate)
import NumericPrelude

import Data.Map (Map)
import qualified Data.Map as Map

import Utils (range, inRange)

type Square = (Int, Int)

squares :: (Int, Int) -> [Square]
squares (width, height) = [(x, y) | x <- range width, y <- range height]

type Delta = (Int, Int)
deltas :: [Delta]
deltas = [(1, 0), (0, 1), (1, 1), (1, -1)]

adjacent :: Square -> [Square]
adjacent square = map (square +) deltas

isValid :: (Int, Int) -> Square -> Bool
isValid (width, height) (x, y) = inRange (0, width) x && inRange (0, height) y

grid :: (Int, Int) -> Map Square [Square]
grid ranges = Map.fromList [(square, filter (isValid ranges) (adjacent square)) | square <- squares ranges]

