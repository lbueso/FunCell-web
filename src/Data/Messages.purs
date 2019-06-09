module Data.Messages where

import Data.Argonaut (class EncodeJson, jsonEmptyObject, (:=), (~>))

data Operation = Operation { operation :: String
                           , path :: String }

instance encodeJsonOperation :: EncodeJson Operation where
  encodeJson (Operation o) = "operation" := o.operation ~>
                             "path" := o.path ~> jsonEmptyObject
