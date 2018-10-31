module SpreadSheet where

import Cell
import Prelude

import Data.Array (mapMaybe, (:))
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a = Update (Tuple Int Int) String a

type State = { spreadSheet :: SpreadSheet Cell
             , cellsInput  :: SpreadSheet String
             , errors      :: Array String }

component :: H.Component HH.HTML Query Unit Void Aff
component = H.component
            { initialState: const (initialState 20 20)
            , render
            , eval
            , receiver: const Nothing
            }

initialState :: Int -> Int -> State
initialState r c =
  { spreadSheet: createSpreadSheet emptyCell r c
  , cellsInput:  createSpreadSheet (\(Tuple _ _) -> "") r c
  , errors: [] }

render :: State -> H.ComponentHTML Query
render state = HH.table_ $ HH.tr_ <$> (spreadSheetToHTML state.spreadSheet)

spreadSheetToHTML :: forall a. SpreadSheet Cell -> Array (Array (HH.HTML a (Query Unit)))
spreadSheetToHTML table = (\x -> (map cellToHTML) <$> x) (toRowsArray table)

cellToHTML :: forall a. Cell -> HH.HTML a (Query Unit)
cellToHTML cell = HH.td_
                  [ HH.input [ HP.type_ HP.InputText
                             , HP.value cell.content
                             , HE.onValueInput (HE.input $ Update (Tuple cell.row cell.col))
                             ]
                  ]

eval :: Query ~> H.ComponentDSL State Query Void Aff
eval (Update (Tuple r c) msg next) = do
  _ <- H.liftEffect $ log $ "cell [" <> show r <> "," <> show c <> "]: " <> msg
  pure next
