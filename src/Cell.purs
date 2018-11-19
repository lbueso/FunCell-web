module Cell where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, getFieldOptional, jsonEmptyArray, jsonEmptyObject, (.?), (:=), (~>))
import Data.Array ((..), fromFoldable, groupBy)
import Data.Array.NonEmpty (toArray)
import Data.Either (Either(..), either)
import Data.Foldable (foldr)
import Data.Map (Map, empty, insert, lookup, values, filterKeys)
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set, member)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup

-- Lib data types
type Row = Int
type Col = Int
type SpreadSheet a = Map (Tuple Row Col) a

type Error  = String
type Result = String

newtype Cell = Cell { row :: Row
                    , col :: Col
                    , content :: Maybe String
                    , evalResult :: Either Error Result }

instance showCell :: Show Cell where
  show (Cell { content: Nothing }) = ""
  show (Cell { evalResult: r, content: (Just c) }) = either show (const c) r

-- Lib functions
createSpreadSheet :: forall a. (Tuple Row Col -> a) -> Row -> Col -> SpreadSheet a
createSpreadSheet e rows columns = foldr f empty keys
    where keys = Tuple <$> (0..rows) <*> (0..columns)
          f x = insert x $ e x

emptyCell :: Tuple Row Col -> Cell
emptyCell (Tuple r c) = Cell { row: r, col: c, content: mempty, evalResult: Right "" }

updateCell :: forall a. Row -> Col -> a -> SpreadSheet a -> SpreadSheet a
updateCell r c = insert (Tuple r c)

updateCellContent :: Row -> Col -> String -> SpreadSheet Cell -> SpreadSheet Cell
updateCellContent r c s table = updateCell r c cell' table
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

getCellContent :: Cell -> Maybe String
getCellContent (Cell c) = c.content

id :: forall a. a -> a
id x = x

-- JSON data types and instances

newtype Message = Message { content :: Array Cell }

instance decodeJsonCell :: DecodeJson Cell where
  decodeJson json = do
    obj <- decodeJson json
    row <- obj .? "row"
    col <- obj .? "col"
    content <- obj .? "content"
    evalResult <- obj .? "evalResult"
    pure $ Cell { row, col, content, evalResult }

instance encodeJsonCell :: EncodeJson Cell where
  encodeJson (Cell c) = "row" := c.row ~>
                        "col" := c.col ~>
                        "content" := c.content ~>
                        "evalResult" := c.evalResult
