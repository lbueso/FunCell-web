module Component.Data where

import Data.Cell (Cell, Col, Row, SpreadSheet)
import Data.Cell.Lib (createSpreadSheet, emptyCell)
import Data.Tuple (Tuple)
import Data.Set (Set)
import Data.Set as S

data Query a = Update (Tuple Row Col) String a
             | UpdateFocus (Tuple Row Col) a
             | Eval (Tuple Row Col) a
             | UpdateResult Cell a

data Message = OutputMessage String

type State = { spreadSheet  :: SpreadSheet Cell
             , selectedCell :: String
             , errors       :: Set (Tuple Row Col) }

initialState :: Int -> Int -> State
initialState r c =
  { spreadSheet: createSpreadSheet emptyCell r c
  , errors: S.empty
  , selectedCell: "" }
