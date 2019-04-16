module Data.Cell where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Either (Either(..), either)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)

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
  show (Cell { evalResult: r, content: (Just c) }) = either (const "ERROR") showRight r
    where showRight "" = c
          showRight x  = x



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
