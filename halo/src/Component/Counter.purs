module Component.Counter where
  
import Prelude

import CSS (AlignContentValue, JustifyContentValue, Value, display, flex, flexDirection, fromString, justifyContent, paddingBottom, paddingLeft, paddingTop, rem, row, width)
import CSS.Common (center)
import Data.Array (range)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HHTML
import Halogen.HTML.CSS as HHTMLCSS
import Halogen.HTML.Events as HHTMLEvents
import Type.Proxy (Proxy(..))

type Input = Int
type Output = Int
type State = { count :: Int }
type SlotNum = Int
type CounterValue = Int

data Query a =
  SetCount Int a
  | GetCount (Int -> a)

type Slots = ( counter :: H.Slot Query Output Int )

data Action =
  Initialize
  | Finalize
  | Increment
  | Decrement
  | FromChild SlotNum CounterValue
  | RaiseParent
  | Received Int

class SpaceEvenly a where
  spaceEvenly :: a

instance SpaceEvenly Value where
  spaceEvenly = fromString "space-evenly"

instance SpaceEvenly AlignContentValue where
  spaceEvenly = fromString "space-evenly"

instance SpaceEvenly JustifyContentValue where
  spaceEvenly = fromString "space-evenly"

_counter = Proxy :: Proxy "counter"

component :: ∀ m. MonadEffect m => Int -> H.Component Query Input Output m
component numChildren = H.mkComponent
  { initialState: \i -> { count: i }
  , render
  , eval: H.mkEval H.defaultEval {
      initialize = Just Initialize
    , finalize = Just Finalize
    , handleAction = handleAction
    , handleQuery = handleQuery
    , receive = Just <<< Received
    }
  }
  where
    handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
    handleAction = case _ of
      Initialize -> log "Initialize"
      Finalize -> log "Finalize"
      Increment -> H.modify_ \s -> s { count = s.count + 1 }
      Decrement -> H.modify_ \s -> s { count = s.count - 1 }
      FromChild slot c -> do
        void $ H.tell _counter slot $ SetCount 123
        count <- H.request _counter slot GetCount
        log $ "Received from Child " <> show (slot + 1) <> ": " <> show c
        log $ "Queried from Child" <> show (slot + 1) <> ": " <> show count
      RaiseParent -> H.get >>= \{ count } -> H.raise count
      Received c -> H.modify_ _ { count = c }
    render { count } = let onClick = HHTMLEvents.onClick <<< const in
      HHTML.div []
      ([
        HHTML.div
        [ HHTMLCSS.style do
            display flex
            flexDirection row
            justifyContent spaceEvenly
            width (rem 6.0)
        ]
        [
          HHTML.button [ onClick Decrement ] [ HHTML.text "-" ]
        , HHTML.text $ show count
        , HHTML.button [ onClick Increment ] [ HHTML.text "+" ]
        ]
      , HHTML.div
        [ HHTMLCSS.style do
            display flex
            justifyContent center
            width (rem 6.0)
            paddingTop (rem 0.5)
        ]
        [ HHTML.button [ onClick RaiseParent ] [ HHTML.text "Raise" ] ]
      ] <> children)
      where
        children = if numChildren == 0 then [] else
          [ HHTML.div
            [ HHTMLCSS.style do
                display flex
                paddingLeft (rem 1.0)
                paddingTop (rem 1.0)
                paddingBottom (rem 1.0)
            ] 
            $ range 0 (numChildren - 1) <#> \n -> HHTML.slot _counter n (component 0) (n + 1) (FromChild n)
          ]

handleQuery :: ∀ a m. MonadEffect m => Query a -> H.HalogenM State Action Slots Output m (Maybe a)
handleQuery = case _ of
  SetCount c a -> H.modify_ _ { count = c } *> pure (Just a)
  GetCount reply -> H.get >>= \{ count } -> pure $ Just $ reply count