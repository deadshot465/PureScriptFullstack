module Component.Page where
  
import Prelude

import CSS (CSS, Color, FontWeight(..), alignItems, backgroundColor, boxShadow, color, cursor, display, fixed, flex, flexBasis, flexDirection, flexEnd, flexGrow, flexShrink, flexStart, fontFamily, fontSize, fontWeight, height, justifyContent, letterSpacing, minHeight, padding, paddingLeft, paddingRight, paddingTop, pct, position, px, rem, rgb, rgba, row, sansSerif, value, vh, white, width, zIndex)
import CSS as CSS
import CSS.Common (center)
import CSS.Cursor (pointer)
import CSS.Text.Shadow (textShadow)
import Capability.Navigate (class Navigate, navigate)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Route (Route)
import Data.Route as Route
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Image.BookCover (bookCover)
import Type.Proxy (Proxy(..))

type State iInput = { iInput :: iInput }

data Action iInput iOutput
  = Input iInput
  | Output iOutput
  | Logoff

type Slots iQuery iOutput = ( inner :: H.Slot iQuery iOutput Unit )

_inner = Proxy :: Proxy "inner"

component
  :: ∀ iInput iOutput iQuery m
  . MonadAff m
  => Navigate m Route
  => H.Component iQuery iInput iOutput m
  -> H.Component iQuery iInput iOutput m
component innerComponent = H.mkComponent
  { initialState: \iInput -> { iInput }
  , render: render innerComponent
  , eval: H.mkEval H.defaultEval
    { handleAction = handleAction
    , handleQuery = handleQuery
    , receive = Just <<< Input
    }
  }

handleAction
  :: ∀ iInput iOutput iQuery m
  . MonadAff m
  => Navigate m Route
  => Action iInput iOutput
  -> H.HalogenM (State iInput) (Action iInput iOutput) (Slots iQuery iOutput) iOutput m Unit
handleAction = case _ of
  Input input -> H.modify_ _ { iInput = input }
  Output output -> H.raise output
  Logoff -> navigate Route.Logoff

handleQuery
  :: ∀ iInput iOutput iQuery m a
  . MonadAff m
  => iQuery a
  -> H.HalogenM (State iInput) (Action iInput iOutput) (Slots iQuery iOutput) iOutput m (Maybe a)
handleQuery = H.query _inner unit

render 
  :: ∀ iInput iOutput iQuery m
  . MonadAff m
  => H.Component iQuery iInput iOutput m
  -> State iInput
  -> H.ComponentHTML (Action iInput iOutput) (Slots iQuery iOutput) m
render innerComponent { iInput } =
  HH.div
    [ HC.style do
        paddingTop $ rem 5.0
    ]
    [ HH.header
        [ HC.style do
            position fixed
            CSS.top $ px 0.0
            width $ pct 100.0
            height $ rem 4.0
            boxShadow (px 10.0) (px 10.0) (px 20.0) (rgba 0 0 24 0.75)
            display flex
            alignItems center
            justifyContent flexStart
            backgroundColor themeColor
            zIndex 1
        ]
        [ HH.div
            [ HC.style do
                paddingLeft $ rem 1.0
                color paperColor
                display flex
                flexDirection row
                alignItems center
                flexBasis $ px 0.0
                flexGrow 1.0
                flexShrink 1.0
            ]
            [ HH.img
                [ HC.style do
                    width $ px 40.0
                , HP.src bookCover
                ]
            , HH.span
                [ HC.style do
                    paddingLeft $ rem 1.0
                    fontSize $ rem 1.3
                    themeFont
                    textShadow (px 0.0) (px 0.0) (px 5.0) (rgba 0 0 0 1.0)
                    fontWeight $ FontWeight $ value "500"
                    letterSpacing $ rem 0.05
                ]
                [ HH.text "Functional Programming Made Easier" ]
            , HH.div
                [ HC.style do
                    display flex
                    flexGrow 1.0
                    justifyContent flexEnd
                    paddingRight $ rem 1.0
                ]
                [ HH.span
                    [ HC.style do
                        themeFont
                        fontWeight $ FontWeight $ value "500"
                        color white
                        padding (rem 1.0) (rem 1.0) (rem 1.0) (rem 1.0)
                        cursor pointer
                    , HE.onClick $ const Logoff
                    ]
                    [ HH.text "Logoff" ]
                ]
            ]
        ]
    , HH.div
        [ HC.style do
            display flex
            alignItems center
            justifyContent center
            minHeight $ vh 90.0       
        ]
        [ HH.slot _inner unit innerComponent iInput Output ]
    ]

paperColor :: Color
paperColor = rgb 0xd9 0xd9 0xd9

themeColor :: Color
themeColor = rgb 0x00 0x66 0x75

themeFont :: CSS
themeFont = fontFamily [ "Verdana" ] $ sansSerif :| []