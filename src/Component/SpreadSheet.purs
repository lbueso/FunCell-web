module Component.SpreadSheet where

import Prelude

import Component.Data (Message(..), Query(..), State, initialState)
import Data.Argonaut (encodeJson, stringify)
import Data.Cell (Cell(..))
import Data.Cell.Lib (clearEvalCell, getCell, id, updateCell, updateCellContent)
import Data.Either (isRight, isLeft)
import Data.Maybe (Maybe(..), maybe)
import Data.Set as S
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Render.Lib (render)

component :: H.Component HH.HTML Query Unit Message Aff
component = H.component
            { initialState: const (initialState 20 20)
            , render
            , eval
            , receiver: const Nothing }

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
