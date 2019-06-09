module Data.Messages where

import Data.Argonaut (class EncodeJson, jsonEmptyObject, (:=), (~>))

data Save = Save String
data Load = Load String

instance encodeJsonSave :: EncodeJson Save where
  encodeJson (Save p) = "save" := p ~> jsonEmptyObject

instance encodeJsonLoad :: EncodeJson Load where
  encodeJson (Load p) = "load" := p ~> jsonEmptyObject
