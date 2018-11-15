module SpreadSheet where

import Cell
import Prelude

import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Coroutine.Aff (emit)
import Control.Monad.Except (runExcept)
import Data.Array (mapMaybe, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Either (either)
import Data.Foldable (for_, foldr)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign (F, Foreign, unsafeToForeign, readString)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.EventTarget as EET
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.WebSocket as WS

data Query a = Update (Tuple Int Int) String a
             | Eval a

type State = { spreadSheet  :: SpreadSheet Cell
             , cellsInput   :: SpreadSheet String
             , selectedCell :: String
             , errors       :: Array String }

data Message = OutputMessage String

component :: H.Component HH.HTML Query Unit Message Aff
component = H.component
            { initialState: const (initialState 100 100)
            , render
            , eval
            , receiver: const Nothing
            }

initialState :: Int -> Int -> State
initialState r c =
  { spreadSheet: createSpreadSheet emptyCell r c
  , cellsInput:  createSpreadSheet (\(Tuple _ _) -> "") r c
  , errors: []
  , selectedCell: "" }

render :: State -> H.ComponentHTML Query
render state = HH.dd_
               [ HH.h1_
                 [ HH.text $ "f(x): " <> state.selectedCell
                 , HH.table_ $ HH.tr_ <$> (spreadSheetToHTML state.spreadSheet)
                 ]
               ]

spreadSheetToHTML :: forall a. SpreadSheet Cell -> Array (Array (HH.HTML a (Query Unit)))
spreadSheetToHTML = map (map cellToHTML) <<< toRowsArray

cellToHTML :: forall a. Cell -> HH.HTML a (Query Unit)
cellToHTML cell = HH.td_
                  [ HH.input [ HP.type_ HP.InputText
                             , HP.value cell.content
                             , HE.onValueInput $ HE.input $ Update (Tuple cell.row cell.col)
                             ]
                  ]

eval :: Query ~> H.ComponentDSL State Query Message Aff
eval (Eval next) = do
  pure next
eval (Update (Tuple r c) msg next) = do
  H.raise $ OutputMessage msg
  state <- H.get
  -- _ <- H.modify (\state -> state { selectedCell: msg } )
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
  _ <- H.liftEffect $ log $ "server: " <> msg
  pure Nothing
