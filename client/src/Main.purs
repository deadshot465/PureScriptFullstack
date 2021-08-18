module Main where

import Prelude

import AppM (runAppM)
import Component.Router as Router
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref as Ref
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = do
  userRef <- Ref.new Nothing
  HA.runHalogenAff do
    body <- HA.awaitBody
    let router = H.hoist (runAppM { userRef }) Router.component
    runUI router unit body
