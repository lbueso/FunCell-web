module Component.SpreadSheet where

import Prelude

import Component.Data (Message(..), Query(..), State, initialState)
import Data.Argonaut (encodeJson, stringify)
import Data.Cell (Cell(..))
import Data.Cell.Lib (clearEvalCell, getCell, updateCell, updateCellContent, getContent, getEvalResult, id, showEval)
import Data.Either (isRight, isLeft, Either(..))
import Data.ExternalModule (ExternalModule(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Set as S
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Render.Lib (render)

component :: H.Component HH.HTML Query Unit Message Aff
component = H.component
            { initialState: const (initialState 35 25)
            , render
            , eval
            , receiver: const Nothing }

eval :: Query ~> H.ComponentDSL State Query Message Aff
eval (Update (Tuple r c) msg next) = do -- updates a cell content on the user's input
  H.modify_ \state -> state { selectedCell = Tuple msg "" }
  H.modify_ \state -> state { spreadSheet = updateCellContent r c msg state.spreadSheet }
  pure next
eval (UpdateFocus (Tuple r c) next) = do -- updates a cell content on the user's focus in
  s <- H.get
  let cell = getCell r c s.spreadSheet
      fx   = maybe mempty getContent cell
      res  = maybe (Right "") (getEvalResult) cell
  H.modify_ \state -> state { selectedCell = Tuple (maybe "" id fx) (showEval res)
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
eval (UpdateExternalModule text next) = do
  H.modify_ \state -> state { externalModule = text }
  pure next
eval (SendExternalModule next) = do
  s <- H.get
  let json = stringify <<< encodeJson $ ExternalModule { text: s.externalModule }
  H.raise $ OutputMessage json
  pure next
