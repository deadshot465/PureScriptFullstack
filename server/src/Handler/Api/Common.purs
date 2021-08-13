module Handler.Api.Common where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Foreign (MultipleErrors)
import Foreign.Generic (class Decode, decodeJSON)
import Handler.Class.ApiHandler (Handler)

handleApi :: ∀ a. Decode a => (a -> Handler) -> String -> Either MultipleErrors Handler
handleApi handler request = do
  req <- runExcept (decodeJSON request :: _ a)
  pure $ handler req