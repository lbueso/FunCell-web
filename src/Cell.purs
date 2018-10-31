module Cell where

import Prelude
import Data.Foldable (foldr)
import Data.Array ((..), fromFoldable, groupBy)
import Data.Array.NonEmpty (NonEmptyArray, toArray)
-- import Data.List (List, (..), groupBy)
-- import Data.List.NonEmpty (NonEmptyList)
import Data.Map (Map, empty, insert, values, lookup)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))

type Row = Int
type Col = Int
type SpreadSheet a = Map (Tuple Row Col) a

type Cell =
  { row :: Row
  , col :: Col
  , content :: String
  }

createSpreadSheet :: forall a. (Tuple Row Col -> a) -> Row -> Col -> SpreadSheet a
createSpreadSheet e rows columns = foldr f empty keys
    where keys = Tuple <$> (0..rows) <*> (0..columns)
          f x = insert x $ e x

emptyCell :: Tuple Row Col -> Cell
emptyCell (Tuple r c) = {row: r, col: c, content: ""}

updateCell :: forall a. Row -> Col -> a -> SpreadSheet a -> SpreadSheet a
updateCell r c = insert (Tuple r c)

-- toRowsList :: SpreadSheet -> List (NonEmptyList Cell)
-- toRowsList = groupBy (\x y -> x.row == y.row) <<< values

toRowsArray :: SpreadSheet Cell -> Array (Array Cell)
toRowsArray = map toArray <<< groupBy (\x y -> x.row == y.row) <<<
              fromFoldable <<< values

getCell :: forall a. Int -> Int -> SpreadSheet a -> Maybe a
getCell r c = lookup (Tuple r c)
