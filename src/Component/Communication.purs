module Component.Communication where

import Prelude

import Component.Data (Message(..), Query(..))
import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Control.Monad.Except (runExcept)
import Data.Argonaut (decodeJson, jsonParser)
import Data.Either (either)
import Data.ExternalModule (ExternalModule(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign (F, Foreign, readString, unsafeToForeign)
import Halogen as H
import Web.Event.EventTarget as EET
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.WebSocket as WS

-- A consumer coroutine that takes output messages from our component
-- IO and sends them using the websocket
wsSender :: WS.WebSocket -> CR.Consumer Message Aff Unit
wsSender socket = CR.consumer \msg -> do
  case msg of
    OutputMessage msgContents ->
      liftEffect $ WS.sendString socket msgContents
  pure Nothing

-- A producer coroutine that emits messages that arrive from the
-- websocket.
wsProducer :: WS.WebSocket -> CR.Producer String Aff Unit
wsProducer socket = CRA.produce \emitter -> do
  listener <- EET.eventListener \ev -> do
    for_ (ME.fromEvent ev) \msgEvent ->
      for_ (readHelper readString (ME.data_ msgEvent)) \msg ->
        emit emitter msg
  EET.addEventListener
    WSET.onMessage
    listener
    false
    (WS.toEventTarget socket)
  where
    readHelper :: forall a b. (Foreign -> F a) -> b -> Maybe a
    readHelper read =
      either (const Nothing) Just <<< runExcept <<< read <<< unsafeToForeign

-- A consumer coroutine that takes the `query` function from our
-- component IO record and sends `AddMessage` queries in when it
-- receives inputs from the producer.
wsConsumer :: (Query ~> Aff) -> CR.Consumer String Aff Unit
wsConsumer query = CR.consumer \msg -> do
  _ <- H.liftEffect $ log $ "RECEIVED: " <> msg
  let json = jsonParser msg >>= \ss -> decodeJson ss
  either
    (H.liftEffect <<< log)
    (query <<< H.action <<< UpdateResult)
    json
  let json = jsonParser msg >>= \ss -> decodeJson ss
  either
    (H.liftEffect <<< log)
    (query <<< H.action <<< UpdateExternalModule <<< (\(ExternalModule t) -> t.text))
    json
  pure Nothing
