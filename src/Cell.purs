module Cell where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, getFieldOptional, jsonEmptyArray, jsonEmptyObject, (.?), (:=), (~>))
import Data.Array ((..), (:), fromFoldable, groupBy, catMaybes)
import Data.Array.NonEmpty (toArray)
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, foldr)
import Data.Map (Map, empty, insert, lookup, values, filterKeys)
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set, member)
import Data.Tuple (Tuple(..))

-- Lib data types
type Row = Int
type Col = Int
type SpreadSheet a = Map (Tuple Row Col) a

type Error  = String
type Result = String

data Cell = Cell { row :: Row
                 , col :: Col
                 , content :: Maybe String
                 , evalResult :: Either Error Result }

instance showCell :: Show Cell where
  show (Cell { content: Nothing }) = ""
  show (Cell { evalResult: r, content: (Just c) }) = either (const c) showRight r
    where showRight "" = c
          showRight x  = x

showError :: Cell -> String
showError (Cell { col: c, row: r, evalResult: (Left res) }) = f res
  where f x = "[" <> (show c) <> "," <> (show r) <> "]: " <> x
showError _ = ""

-- showErrors :: forall a. Foldable a => a (Tuple Row Col) -> SpreadSheet a -> String
showErrors :: forall a b. Ord a => Foldable b => Map a Cell -> b a -> Array String
showErrors table = map showError <<< catMaybes <<< foldr f []
  where f x acc = lookup x table : acc

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

-- toRowsList :: forall a. SpreadSheet (Cell a) -> List (NonEmptyList (Cell a))
-- toRowsList = groupBy (\x y -> x.row == y.row) <<< values

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

id :: forall a. a -> a
id x = x

-- JSON data types and instances
instance decodeJsonCell :: DecodeJson Cell where
  decodeJson json = do
    obj <- decodeJson json
    row <- obj .? "row"
    col <- obj .? "col"
    content <- obj .? "content"
    evalCon <- obj .? "evalResult"
    evalObj <- decodeJson evalCon
    evalResult <- (Left  <$> evalObj .? "Left") <|>
                  (Right <$> evalObj .? "Right")
    pure $ Cell { row, col, content, evalResult }

instance encodeJsonCell :: EncodeJson Cell where
  encodeJson (Cell c) = "row" := c.row ~>
                        "col" := c.col ~>
                        "content" := c.content ~>
                        "evalResult" := (eitherEncode c.evalResult ~> jsonEmptyObject) ~>
                        jsonEmptyObject
    where eitherEncode = either (\x -> "Left" := x) (\x -> "Right" := x)
