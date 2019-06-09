module Render.Lib where

import Prelude

import Component.Data (Query(..), State)
import Data.Array (catMaybes, head, length, singleton, tail, zipWith, (..))
import Data.Cell (Cell(..), SpreadSheet)
import Data.Cell.Lib (toRowsArray, id)
import Data.Char (fromCharCode)
import Data.Maybe (maybe)
import Data.String.CodeUnits as S
import Data.Tuple (Tuple(..), fst, snd)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

renderError :: forall a b. String -> HH.HTML a b
renderError error = HH.li_ [ HH.text error ]

renderSpreadSheet :: forall a. SpreadSheet Cell -> Array (HH.HTML a (Query Unit))
renderSpreadSheet = map HH.tr_ <<< appendColumnHeaders <<< appendRowsHeaders
                    <<< map (map renderCell) <<< toRowsArray
  where appendColumnHeaders xs = [ renderColumnHeaders $ columns xs ] <> xs
        appendRowsHeaders   xs = zipWith (\x y -> append [x] y) (map renderRowHeader
                                                                 $ 0 .. (length xs)) xs
        columns = maybe 0 (length) <<< head

renderColumnHeaders :: forall a. Int -> Array (HH.HTML a (Query Unit))
renderColumnHeaders cols = append [HH.th_ [] ] <<< map renderColumnHeader <<< catMaybes
                           <<< map fromCharCode $ (65 .. (63 + cols)) -- TODO

renderColumnHeader :: forall a. Char -> HH.HTML a (Query Unit)
renderColumnHeader x = HH.th_ [ HH.text $ S.singleton x ]

renderRowHeader :: forall a. Int -> HH.HTML a (Query Unit)
renderRowHeader x = HH.td_ [ HH.text $ show x ]

renderCell :: forall a. Cell -> HH.HTML a (Query Unit)
renderCell cell@(Cell c) = HH.td_
                  [ HH.input [ HP.type_ HP.InputText
                             , HP.value $ show cell
                             , HE.onValueInput $ HE.input  $ Update (Tuple c.row c.col)
                             , HE.onFocusIn    $ HE.input_ $ UpdateFocus (Tuple c.row c.col)
                             , HE.onFocusOut   $ HE.input_ $ Eval (Tuple c.row c.col)
                             ]
                  ]

render :: State -> H.ComponentHTML Query
render state = HH.div [ HP.class_ (HH.ClassName "container") ]
               [ HH.div
                 [ HP.class_ (HH.ClassName "formula-wrapper") ]
                 [ HH.text $ "fx : " <> fst state.selectedCell ]
               , HH.div
                 [ HP.class_ (HH.ClassName "table-wrapper")]
                 [ HH.table_ [ HH.thead_ $ maybe [] singleton (head spreadSheetHTML),
                               HH.tbody_ $ maybe [] id (tail spreadSheetHTML) ] ]
               , HH.div
                 [ HP.class_ (HH.ClassName "error-wrapper")]
                 [ HH.text $ snd state.selectedCell ]
               , HH.div
                 [ HP.class_ (HH.ClassName "text-input-wrapper") ]
                 [ HH.textarea [ HE.onValueInput $ HE.input  $ UpdateExternalModule
                               , HE.onFocusOut   $ HE.input_ $ SendExternalModule
                               , HP.value $ state.externalModule ] ]
               , HH.div
                 [ HP.class_ (HH.ClassName "save-load-wrapper") ]
                 [ HH.div
                   [ HP.class_ (HH.ClassName "path-wrapper") ]
                   [ HH.textarea [ HE.onValueInput $ HE.input  $ UpdateFilePath ] ]
                 , HH.div
                   [ HP.class_ (HH.ClassName "buttons-wrapper") ]
                   [ HH.button [ HE.onClick (HE.input_ SaveFile) ] [HH.text "save"]
                   , HH.button [ HE.onClick (HE.input_ LoadFile) ] [HH.text "load"]
                   ] ] ]
  where spreadSheetHTML = renderSpreadSheet state.spreadSheet
