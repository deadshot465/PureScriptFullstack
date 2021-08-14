module Main where

import Prelude

import Component.CountDownTimer as CountDownTimer
import Component.Counter as Counter
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen.Aff as HAff
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = do
  HAff.runHalogenAff do
    body <- HAff.awaitBody
    io <- runUI (Counter.component 3) 0 body
    void $ liftEffect $ HS.subscribe io.messages \currentCount ->
      log $ "Received Count from top-level counter: " <> show currentCount
    ioTimer <- runUI CountDownTimer.component 15 body
    void $ liftEffect $ HS.subscribe ioTimer.messages \_ -> launchAff_ io.dispose