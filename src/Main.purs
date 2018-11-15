module Main where

import Prelude
import SpreadSheet

import Control.Coroutine as CR
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.Socket.WebSocket as WS

main :: Effect Unit
main = do
  connection <- WS.create "ws://127.0.0.1:9160" []
  HA.runHalogenAff do
    body <- HA.awaitBody
    io <- runUI component unit body

    -- The wsSender consumer subscribes to all output messages from
    -- our component
    io.subscribe $ wsSender connection

    -- Connecting the consumer to the producer initializes both,
    -- feeding queries back to our component as messages are received.
    CR.runProcess (wsProducer connection CR.$$ wsConsumer io.query)
