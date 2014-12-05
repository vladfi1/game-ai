module Types where
import Data.Packed.Matrix
import Data.Packed.Vector
import NewGame

type Datum = Vector Double
type Data = Matrix Double

type DataSet = [(Datum, Datum)]

type Saved s = ([(s, Action s)], s)
