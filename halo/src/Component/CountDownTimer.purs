module Component.CountDownTimer where
  
import Prelude

import CSS (column, display, flex, flexDirection, padding, rem)
import Control.Alt ((<|>))
import Control.Monad.Rec.Class (forever)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), delay, error, forkAff, killFiber, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HHTML
import Halogen.HTML.CSS as HHTMLCSS
import Halogen.Query.Event as HQE
import Halogen.Query.HalogenM as HQ
import Halogen.Subscription as HS
import Web.DOM.Document as DomDocument
import Web.DOM.Element as DomElement
import Web.DOM.Node as DomNode
import Web.Event.Event as WebEvent
import Web.HTML (HTMLDocument, window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes (click)

type Input = Int
type Output = Unit

type State =
  { count :: Int
  -- , tickFiber' :: Maybe (Fiber Unit)
  }

type Slots :: ∀ k. Row k
type Slots = ()

type Query :: ∀ k. k -> Type
type Query = Const Void

data Action =
  Initialize
  | Tick H.SubscriptionId
  -- | Finalize
  | HandleDocClick HTMLDocument MouseEvent

component :: ∀ m. MonadAff m => H.Component Query Int Unit m
component = H.mkComponent
  { initialState: \i ->
    { count: i
    -- , tickFiber': Nothing
    }
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
  {- Initialize -> do
    { emitter: tickEmitter, listener: tickListener } <- H.liftEffect HS.create
    tickSubscription <- HQ.subscribe tickEmitter
    tickFiber <- H.liftAff $ forkAff $ forever do
      delay $ Milliseconds 1000.0
      H.liftEffect $ HS.notify tickListener $ Tick tickSubscription
    H.modify_ _ { tickFiber' = Just tickFiber } -}
  Initialize -> do
    -- Timer setup
    { emitter: tickEmitter, listener: tickListener } <- H.liftEffect HS.create
    tickFiber <- H.liftAff $ forkAff $ forever do
      log "Sleeping..."
      delay $ Milliseconds 1000.0
      H.liftEffect $ HS.notify tickListener Tick
    let fiberEmitter = HS.makeEmitter \_ -> pure $ launchAff_ do
          log "Killing tick fiber..."
          killFiber (error "Event source finalized") tickFiber
    HQ.subscribe' \sid -> (tickEmitter <|> fiberEmitter) <#> (_ $ sid)
    -- Document click setup
    document <- H.liftEffect $ Window.document =<< window
    HQ.subscribe' $ const $ HQE.eventListener
      click
      (HTMLDocument.toEventTarget document)
      (map (HandleDocClick document) <<< ME.fromEvent)
  Tick sid -> do
    { count } <- H.get
    unless (count <= 0) $ H.modify_ _ { count = count - 1 }
    when (count == 1) do
      H.raise unit
      H.unsubscribe sid
      -- killTickFiber
  HandleDocClick document event -> H.liftEffect do
    docNode' <- map DomElement.toNode <$> (DomDocument.documentElement $ HTMLDocument.toDocument document)
    let clickNode' = DomElement.toNode <$> (DomElement.fromEventTarget =<< (WebEvent.target $ ME.toEvent event))
    case docNode', clickNode' of
      Just dn, Just cn -> do
        eq <- DomNode.isEqualNode dn cn
        when eq $ log "Document clicked on"
      _, _ -> pure unit
  {- Finalize -> killTickFiber
  where
    killTickFiber = do
      { tickFiber' } <- H.get
      tickFiber' # maybe (pure unit) \tickFiber -> do
        H.liftAff $ killFiber (error "Event source finalized") tickFiber
        H.modify_ _ { tickFiber' = Nothing } -}