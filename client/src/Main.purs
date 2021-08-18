module Main where

import Prelude

import AppM (runAppM)
import Component.Router as Router
import Data.Maybe (Maybe(..))
import Data.Route (routeCodec)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Ref as Ref
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.Duplex as RouteDuplex
import Routing.Hash (matchesWith)

main :: Effect Unit
main = do
  userRef <- Ref.new Nothing
  HA.runHalogenAff do
    body <- HA.awaitBody
    let router = H.hoist (runAppM { userRef }) Router.component
    io <- runUI router unit body
    liftEffect $ matchesWith (RouteDuplex.parse routeCodec)
      \old' new -> when (old' /= Just new) $ launchAff_ $ io.query $ H.mkTell $ Router.Navigate new
