module Component.Data where

import Data.Cell (Cell, Col, Row, SpreadSheet)
import Data.Cell.Lib (createSpreadSheet, emptyCell)
import Data.Tuple (Tuple(..))

data Query a = Update (Tuple Row Col) String a
             | UpdateFocus (Tuple Row Col) a
             | Eval (Tuple Row Col) a
             | UpdateResult Cell a
             | UpdateExternalModule String a
             | SendExternalModule a
             | UpdateFilePath String a
             | SaveFile a
             | LoadFile a

data Message = OutputMessage String

type State = { spreadSheet    :: SpreadSheet Cell
             , selectedCell   :: Tuple String String
             , externalModule :: String
             , filePath       :: String }

initialState :: Int -> Int -> State
initialState r c =
  { spreadSheet: createSpreadSheet emptyCell r c
  , selectedCell: Tuple "" ""
  , externalModule: "module ExternalModule where"
  , filePath: "" }
