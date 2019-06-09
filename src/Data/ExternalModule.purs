module Data.ExternalModule where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.?), (:=), (~>))

data ExternalModule = ExternalModule { text :: String }

instance encodeJsonExternalModule :: EncodeJson ExternalModule where
  encodeJson (ExternalModule t) = "text" := t.text ~> jsonEmptyObject

instance decodeJsonExternalModule :: DecodeJson ExternalModule where
  decodeJson json = do
    obj  <- decodeJson json
    text <- obj .? "text"
    pure $ ExternalModule { text }
