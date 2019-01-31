module Render.Lib where

import Prelude

import Component.Data (Query(..), State)
import Data.Array (catMaybes, head, length, singleton, tail, zipWith, (..))
import Data.Cell (Cell(..), SpreadSheet)
import Data.Cell.Lib (showErrors, toRowsArray)
import Data.Char (fromCharCode)
import Data.Maybe (maybe)
import Data.String.CodeUnits as S
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

renderError :: forall a b. String -> HH.HTML a b
renderError error = HH.li_ [ HH.text error ]

renderSpreadSheet :: forall a. SpreadSheet Cell -> Array (HH.HTML a (Query Unit))
renderSpreadSheet = map HH.tr_ <<< appendColumnHeaders <<< appendRowsHeaders
                    <<< map (map renderCell) <<< toRowsArray
  where appendColumnHeaders xs = [renderColumnHeaders $ length xs] <> xs
        appendRowsHeaders   xs = zipWith (\x y -> append [x] y) (map renderRowHeader $ 0 .. (length xs)) xs

renderColumnHeaders :: forall a. Int -> Array (HH.HTML a (Query Unit))
renderColumnHeaders rows = append [HH.th_ [] ] <<< map renderColumnHeader <<< catMaybes
                           <<< map fromCharCode $ (65 .. (64 + rows)) -- TODO

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
render state = HH.dd_
               [ HH.h1_
                 [ HH.div_ [ HH.text $ "f(x): " <> state.selectedCell ]
                 , HH.div_ [ HH.ul_ (map renderError
                                     $ showErrors state.spreadSheet state.errors)]
                 , HH.div
                   [ HP.class_ (HH.ClassName "table-wrapper")]
                   [ HH.table_ [ HH.thead_ $ maybe [] singleton (head spreadSheetHTML),
                                 HH.tbody_ $ maybe [] id (tail spreadSheetHTML) ] ]
                 ]
               ]
  where spreadSheetHTML = renderSpreadSheet state.spreadSheet

css :: forall r i. String -> HH.IProp ( class :: String | r ) i
css = HP.class_ <<< HH.ClassName

id :: forall a. a -> a
id x = x
