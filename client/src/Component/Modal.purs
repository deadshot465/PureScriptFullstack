module Component.Modal where
  
import Prelude

import AppTheme (paperColor, themeColor, themeFont)
import CSS (FontWeight(..), Size(..), alignItems, backgroundColor, color, column, display, fixed, flex, flexDirection, flexEnd, fontSize, fontWeight, height, justifyContent, left, marginLeft, maxWidth, minHeight, minWidth, padding, pct, position, rem, rgba, row, spaceAround, top, value, white, width, zIndex)
import CSS.Common (center)
import CSS.Overflow (overflow, overflowAuto)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))

type State iInput = { iInput :: iInput }

data Output iOutput
  = Affirmative
  | Negative
  | InnerOutput iOutput

data Action iInput iOutput
  = Input iInput
  | Output iOutput
  | AffirmativeClicked
  | NegativeClicked

type Slots iQuery iOutput = ( inner :: H.Slot iQuery iOutput Unit )

_inner = Proxy :: Proxy "inner"

component
  :: ∀ iQuery iInput iOutput m
  . MonadAff m
  => H.Component iQuery iInput iOutput m
  -> H.Component iQuery iInput (Output iOutput) m
component innerComponent = H.mkComponent
  { initialState: \iInput -> { iInput }
  , render: render innerComponent
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , receive = Just <<< Input
      }
  }

handleAction
  :: ∀ iInput iOutput iQuery m
  . MonadAff m
  => Action iInput iOutput
  -> H.HalogenM (State iInput) (Action iInput iOutput) (Slots iQuery iOutput) (Output iOutput) m Unit
handleAction = case _ of
  Input input -> H.modify_ _ { iInput = input }
  Output output -> H.raise $ InnerOutput output
  AffirmativeClicked -> H.raise Affirmative
  NegativeClicked -> H.raise Negative

render
  :: ∀ iInput iOutput iQuery m
  . MonadAff m
  => H.Component iQuery iInput iOutput m
  -> State iInput
  -> H.ComponentHTML (Action iInput iOutput) (Slots iQuery iOutput) m
render innerComponent { iInput } =
  HH.div
    [ HC.style do
        display flex
        alignItems center
        justifyContent center
        position fixed
        top $ Size $ value 0.0
        left $ Size $ value 0.0
        width (pct 100.0)
        height (pct 100.0)
        overflow overflowAuto
        backgroundColor (rgba 0 0 0 0.4)
        zIndex 1
    ]
    [ HH.div
      [ HC.style do
          display flex
          flexDirection column
          justifyContent spaceAround
          padding (rem 1.0) (rem 1.0) (rem 1.0) (rem 1.0)
          backgroundColor paperColor
          minWidth (rem 40.0)
          maxWidth (pct 80.0)
          minHeight (rem 10.0)
      ]
      [ HH.div
        [ HC.style do
            display flex
            flexDirection column
            padding (rem 1.0) (rem 1.0) (rem 1.0) (rem 1.0)
        ]
        [ HH.slot _inner unit innerComponent iInput Output ]
      , HH.div
          [ HC.style do
              display flex
              flexDirection row
              alignItems center
              justifyContent flexEnd
              width (pct 100.0)
              backgroundColor paperColor
          ]
          [ HH.button
              [ buttonStyle
              , HE.onClick $ const AffirmativeClicked
              ]
              [ HH.text "OK" ]
          , HH.button
              [ buttonStyle
              , HE.onClick $ const NegativeClicked
              ]
              [ HH.text "CANCEL" ]
          ]
      ]
    ]
  where
    buttonStyle = HC.style do
      themeFont
      fontWeight $ FontWeight $ value "500"
      color white
      padding (rem 0.5) (rem 0.5) (rem 0.5) (rem 0.5)
      backgroundColor themeColor
      width (rem 8.0)
      marginLeft (rem 2.0)
      fontSize (rem 0.9)

handleQuery
  :: ∀ iInput iOutput iQuery a m
  . MonadAff m
  => iQuery a
  -> H.HalogenM (State iInput) (Action iInput iOutput) (Slots iQuery iOutput) (Output iOutput) m (Maybe a)
handleQuery = H.query _inner unit