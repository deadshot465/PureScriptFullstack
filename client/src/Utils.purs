module Utils where

import Prelude

import Affjax as Ajax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign.Generic (class Decode, class Encode, decodeJSON, encodeJSON)

apiCall
  :: ∀ request response m
  . MonadAff m
  => Encode request
  => Decode response
  => request
  -> m (Either String response)
apiCall request = do
  ajaxResult <- liftAff $ Ajax.post ResponseFormat.string
    "http://localhost:3000/"
    $ Just $ RequestBody.String
    $ encodeJSON request
  pure do
    { body } <- lmap Ajax.printError ajaxResult
    lmap show $ runExcept $ decodeJSON body