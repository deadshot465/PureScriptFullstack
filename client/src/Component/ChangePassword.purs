module Component.ChangePassword where
  
import Prelude

import AppTheme (themeColor, themeFont)
import CSS (FontWeight(..), alignItems, backgroundColor, color, cursor, display, flex, flexDirection, fontSize, fontWeight, height, justifyContent, paddingTop, rem, row, value, vw, white, width)
import CSS.Common (center)
import CSS.Cursor (pointer)
import Capability.Navigate (class Navigate, navigate)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Route (Route)
import Data.Route as Route
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE

type State = {}
type Input = Unit
type Output = Void

data Action
  = Click

type Query :: ∀ k. k -> Type
type Query = Const Void

type Slots :: ∀ k. Row k
type Slots = ()

component
  :: ∀ m
  . MonadAff m
  => Navigate m Route
  => H.Component Query Input Output m
component = H.mkComponent
  { initialState: \_ -> {}
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction

      }
  }

render
  :: ∀ m
  . MonadAff m
  => Navigate m Route
  => State
  -> H.ComponentHTML Action Slots m
render _ =
  HH.div
    [ HC.style do
        display flex
        flexDirection row
        alignItems center
        justifyContent center
        paddingTop $ vw 0.65
    ]
    [ HH.button
        [ HC.style do
            backgroundColor themeColor
            themeFont
            fontWeight $ FontWeight $ value "500"
            fontSize $ vw 1.0
            width (rem 20.0)
            height $ vw 3.0
            color white
            cursor pointer
        , HE.onClick $ const Click
        ]
        [ HH.text "Users" ]
    ]

handleAction
  :: ∀ m
  . MonadAff m
  => Navigate m Route
  => Action
  -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
  Click -> navigate $ Route.Users Nothing