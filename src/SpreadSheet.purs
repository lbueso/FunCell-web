module SpreadSheet where

import Cell
import Prelude

import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Control.Monad.Except (runExcept)
import Data.Argonaut (decodeJson, encodeJson, fromString, stringify, toArray, jsonParser)
import Data.Array ((:), filter)
import Data.Either (Either, either, isRight, isLeft, fromLeft)
import Data.Foldable (for_, foldr)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Set (Set)
import Data.Set as S
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign (F, Foreign, readString, typeOf, unsafeToForeign)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.EventTarget as EET
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.WebSocket as WS

data Query a = Update (Tuple Row Col) String a
             | UpdateFocus (Tuple Row Col) a
             | Eval (Tuple Row Col) a
             | UpdateResult Cell a

type State = { spreadSheet  :: SpreadSheet Cell
             , selectedCell :: String
             , errors       :: Set (Tuple Row Col) }

data Message = OutputMessage String

component :: H.Component HH.HTML Query Unit Message Aff
component = H.component
            { initialState: const (initialState 20 20)
            , render
            , eval
            , receiver: const Nothing }

initialState :: Int -> Int -> State
initialState r c =
  { spreadSheet: createSpreadSheet emptyCell r c
  , errors: S.empty
  , selectedCell: "" }

render :: State -> H.ComponentHTML Query
render state = HH.dd_
               [ HH.h1_
                 [ HH.div_ [ HH.text $ "f(x): " <> state.selectedCell ]
                 , HH.div_ [ HH.ul_ (map renderError
                                     $ showErrors state.spreadSheet state.errors)]
                 , HH.div_ [ HH.style_ [] -- TODO
                           , HH.table_ $ HH.tr_ <$> (spreadSheetToHTML state.spreadSheet)]
                 ]
               ]

renderError :: forall a b. String -> HH.HTML a b
renderError error = HH.li_ [ HH.text error ]

spreadSheetToHTML :: forall a. SpreadSheet Cell-> Array (Array (HH.HTML a (Query Unit)))
spreadSheetToHTML = map (map cellToHTML) <<< toRowsArray

cellToHTML :: forall a. Cell -> HH.HTML a (Query Unit)
cellToHTML cell@(Cell c) = HH.td_
                  [ HH.input [ HP.type_ HP.InputText
                             , HP.value $ show cell
                             , HE.onValueInput $ HE.input  $ Update (Tuple c.row c.col)
                             , HE.onFocusIn    $ HE.input_ $ UpdateFocus (Tuple c.row c.col)
                             , HE.onFocusOut   $ HE.input_ $ Eval (Tuple c.row c.col)
                             ]
                  ]

eval :: Query ~> H.ComponentDSL State Query Message Aff
eval (Update (Tuple r c) msg next) = do -- updates a cell content on the user's input
  H.modify_ \state -> state { selectedCell = msg }
  H.modify_ \state -> state { spreadSheet = updateCellContent r c msg state.spreadSheet }
  pure next
eval (UpdateFocus (Tuple r c) next) = do -- updates a cell content on the user's focus in
  s <- H.get
  let msg = getCell r c s.spreadSheet >>= \(Cell cell) -> cell.content
  H.modify_ \state -> state { selectedCell = maybe "" id msg
                            , spreadSheet = clearEvalCell r c state.spreadSheet }
  pure next
eval (Eval (Tuple r c) next) = do
  s <- H.get
  let cell = getCell r c s.spreadSheet
      json = maybe "" (stringify <<< encodeJson) cell
  when (json /= "") $ H.raise $ OutputMessage json
  pure next
eval (UpdateResult cell@(Cell c) next) = do -- updates the cell content after eval
  H.modify_ \state -> state { spreadSheet = updateCell cell state.spreadSheet }
  when (isLeft c.evalResult) $
    H.modify_ \state -> state { errors = S.insert (Tuple c.row c.col) state.errors }
  when (isRight c.evalResult) $
    H.modify_ \state -> state { errors = S.delete (Tuple c.row c.col) state.errors }
  pure next

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
  pure Nothing
