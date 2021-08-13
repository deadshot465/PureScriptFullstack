module Util where

import Prelude

import Control.Monad.Except (ExceptT, except, lift)
import Data.Either (Either)
import Data.Tuple (Tuple(..))
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)

withAVar :: ∀ a b m. MonadAff m => AVar a -> (a -> m (Tuple a b)) -> m b
withAVar aVar f = do
  value <- liftAff $ AVar.take aVar
  Tuple newValue result <- f value
  liftAff $ AVar.put newValue aVar
  pure result

liftSuccess :: ∀ a m e. Monad m => m (Either e a) -> ExceptT e m a
liftSuccess ma = ma # lift >>= except