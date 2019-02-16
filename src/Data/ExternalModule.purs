module Data.ExternalModule where

import Data.Argonaut (class EncodeJson, jsonEmptyObject, (:=), (~>))

data ExternalModule = ExternalModule { text :: String }

instance encodeJsonExternalModule :: EncodeJson ExternalModule where
  encodeJson (ExternalModule t) = "text" := t.text ~> jsonEmptyObject
