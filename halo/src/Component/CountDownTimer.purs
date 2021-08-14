module Component.CountDownTimer where
  
import Prelude

import CSS (column, display, flex, flexDirection, padding, rem)
import Control.Monad.Rec.Class (forever)
import Data.Const (Const)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Fiber, Milliseconds(..), delay, error, forkAff, killFiber)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HHTML
import Halogen.HTML.CSS as HHTMLCSS
import Halogen.Query.HalogenM as HQ
import Halogen.Subscription as HS

type Input = Int
type Output = Unit

type State = { count :: Int, tickFiber' :: Maybe (Fiber Unit) }

type Slots :: ∀ k. Row k
type Slots = ()

type Query :: ∀ k. k -> Type
type Query = Const Void

data Action =
  Initialize
  | Finalize
  | Tick H.SubscriptionId

component :: ∀ m. MonadAff m => H.Component Query Int Unit m
component = H.mkComponent
  { initialState: \i -> { count: i, tickFiber': Nothing }
  , render
  , eval: H.mkEval H.defaultEval
    { initialize = Just Initialize
    , handleAction = handleAction
    }
  }
  where
    render { count } =
      HHTML.div
        [ HHTMLCSS.style do
            display flex
            flexDirection column
            padding (rem 1.0) (rem 1.0) (rem 1.0) (rem 1.0)
        ]
        [ HHTML.text $ "COUNTDOWN: " <> show count ]

handleAction :: ∀ m. MonadAff m => Action -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
  Initialize -> do
    { emitter: tickEmitter, listener: tickListener } <- H.liftEffect HS.create
    tickSubscription <- HQ.subscribe tickEmitter
    tickFiber <- H.liftAff $ forkAff $ forever do
      delay $ Milliseconds 1000.0
      H.liftEffect $ HS.notify tickListener $ Tick tickSubscription
    H.modify_ _ { tickFiber' = Just tickFiber }
    pure unit
  Tick sid -> do
    { count } <- H.get
    unless (count <= 0) $ H.modify_ _ { count = count - 1 }
    when (count == 1) do
      H.raise unit
      H.unsubscribe sid
      killTickFiber
  Finalize -> killTickFiber
  where
    killTickFiber = do
      { tickFiber' } <- H.get
      tickFiber' # maybe (pure unit) \tickFiber -> do
        H.liftAff $ killFiber (error "Event source finalized") tickFiber
        H.modify_ _ { tickFiber' = Nothing }