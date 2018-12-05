module Render.Lib where

import Prelude

import Component.Data (Query(..), State)
import Data.Cell (Cell(..), SpreadSheet)
import Data.Cell.Lib (showErrors, toRowsArray)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

renderError :: forall a b. String -> HH.HTML a b
renderError error = HH.li_ [ HH.text error ]

renderSpreadSheet :: forall a. SpreadSheet Cell -> Array (Array (HH.HTML a (Query Unit)))
renderSpreadSheet = map (map renderCell) <<< toRowsArray

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
                 , HH.div_ [ HH.style_ [] -- TODO
                           , HH.table_ $ HH.tr_ <$> (renderSpreadSheet state.spreadSheet)]
                 ]
               ]
