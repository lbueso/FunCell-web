module Data.Cell.Lib where

import Prelude

import Data.Array (fromFoldable, groupBy, (..), catMaybes, (:))
import Data.Array.NonEmpty (toArray)
import Data.Cell (Cell(..), Col, Row, SpreadSheet)
import Data.Char (fromCharCode)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldr)
import Data.String.CodeUnits (singleton)
import Data.Map (Map, lookup, empty, filterKeys, insert, values)
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set, member)
import Data.Tuple (Tuple(..))

-- Lib functions
createSpreadSheet :: forall a. (Tuple Row Col -> a) -> Row -> Col -> SpreadSheet a
createSpreadSheet e rows columns = foldr f empty keys
    where keys = Tuple <$> (0..rows) <*> (0..columns)
          f x = insert x $ e x

emptyCell :: Tuple Row Col -> Cell
emptyCell (Tuple r c) = Cell { row: r, col: c, content: mempty, evalResult: Right "" }

updateCellVal :: forall a. Row -> Col -> a -> SpreadSheet a -> SpreadSheet a
updateCellVal r c = insert (Tuple r c)

updateCellContent :: Row -> Col -> String -> SpreadSheet Cell -> SpreadSheet Cell
updateCellContent r c s table = updateCellVal r c cell' table
  where (Cell cell) = maybe (emptyCell $ Tuple r c) id (getCell r c table)
        cell' = Cell $ cell { content = Just s }

toRowsArray :: SpreadSheet Cell -> Array (Array Cell)
toRowsArray = map toArray <<< groupBy (\(Cell x) (Cell y) -> x.row == y.row) <<<
              fromFoldable <<< values

getCell :: forall a. Int -> Int -> SpreadSheet a -> Maybe a
getCell r c = lookup (Tuple r c)

getCells :: forall a. Set (Tuple Row Col) -> SpreadSheet a -> Array a
getCells keys = fromFoldable <<< filterKeys f
  where f k = member k keys

updateCell :: Cell -> SpreadSheet Cell -> SpreadSheet Cell
updateCell (Cell c) = updateCellVal c.row c.col (Cell c)

updateCells :: SpreadSheet Cell -> Array Cell -> SpreadSheet Cell
updateCells = foldr updateCell

clearEval :: Cell -> Cell
clearEval (Cell c) = Cell $ c { evalResult = Right "" }

clearEvalCell :: Row -> Col -> SpreadSheet Cell -> SpreadSheet Cell
clearEvalCell r c table = maybe table f $ getCell r c table
  where f cell = updateCellVal r c (clearEval cell) table

showError :: Cell -> String
showError (Cell { col: c, row: r, evalResult: (Left res) }) = f res
  where f x = (toColumn c) <> (show r) <> ": " <> x
showError _ = ""

toColumn :: Int -> String
toColumn = maybe "" (singleton) <<< fromCharCode <<< (+) 65 -- TODO

showErrors :: forall a b. Ord a => Foldable b => Map a Cell -> b a -> Array String
showErrors table = map showError <<< catMaybes <<< foldr f []
  where f x acc = lookup x table : acc

id :: forall a. a -> a
id x = x
