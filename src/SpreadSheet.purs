module SpreadSheet where

import Cell
import Prelude

import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Control.Monad.Except (runExcept)
import Data.Argonaut (decodeJson, encodeJson, fromString, stringify, toArray)
import Data.Array ((:))
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
             | Eval a
             | UpdateResult (Array Cell) a

type State = { spreadSheet  :: SpreadSheet Cell
             , selectedCell :: String
             , toEval       :: Set (Tuple Row Col)
             , errors       :: Array String }

data Message = OutputMessage String

component :: H.Component HH.HTML Query Unit Message Aff
component = H.component
            { initialState: const (initialState 20 10)
            , render
            , eval
            , receiver: const Nothing }

initialState :: Int -> Int -> State
initialState r c =
  { spreadSheet: createSpreadSheet emptyCell r c
  , errors: []
  , toEval: S.empty
  , selectedCell: "" }

render :: State -> H.ComponentHTML Query
render state = HH.dd_
               [ HH.h1_
                 [ HH.text $ "f(x): " <> state.selectedCell
                 , HH.table_ $ HH.tr_ <$> (spreadSheetToHTML state.spreadSheet)
                 ]
               ]

spreadSheetToHTML :: forall a. SpreadSheet Cell-> Array (Array (HH.HTML a (Query Unit)))
spreadSheetToHTML = map (map cellToHTML) <<< toRowsArray

cellToHTML :: forall a. Cell -> HH.HTML a (Query Unit)
cellToHTML cell@(Cell c) = HH.td_
                  [ HH.input [ HP.type_ HP.InputText
                             , HP.value $ show cell
                             , HE.onValueInput $ HE.input  $ Update (Tuple c.row c.col)
                             , HE.onFocusIn    $ HE.input_ $ UpdateFocus (Tuple c.row c.col)
                             , HE.onFocusOut   $ HE.input_ $ Eval
                             ]
                  ]

eval :: Query ~> H.ComponentDSL State Query Message Aff
eval (Update (Tuple r c) msg next) = do
  H.modify_ \state -> state { selectedCell = msg }
  H.modify_ \state -> state { spreadSheet = updateCellContent r c msg state.spreadSheet }
  pure next
eval (UpdateFocus (Tuple r c) next) = do
  s <- H.get
  let msg = getCell r c s.spreadSheet >>= \(Cell cell) -> cell.content
  H.modify_ \state -> state { selectedCell = maybe "" id msg
                            , toEval = S.insert (Tuple r c) state.toEval }
  pure next
eval (Eval next) = do
  s <- H.get
  let cells = getCells s.toEval s.spreadSheet
  let json  = stringify $ encodeJson cells
  H.raise $ OutputMessage json
  H.modify_ \state -> state { toEval = (S.empty :: Set (Tuple Row Col)) }
  pure next
eval (UpdateResult cells next) = do
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
  let json = decodeJson (fromString msg) >>= traverse decodeJson -- TODO not working...
  either
    (H.liftEffect <<< log)
    (query <<< H.action <<< UpdateResult)
    json
  pure Nothing
