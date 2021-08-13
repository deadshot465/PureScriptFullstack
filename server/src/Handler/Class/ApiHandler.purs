module Handler.Class.ApiHandler where

import Control.Monad.Reader (ReaderT)
import Data.Either (Either)
import Effect.AVar (AVar)
import Effect.Aff (Aff)
import Entity.Account (Accounts)
import Foreign (MultipleErrors)
import HTTPure (Response)
import Type.Proxy (Proxy)

class ApiHandler :: ∀ k. k -> Constraint
class ApiHandler a where
  handle :: String -> Proxy a -> Either MultipleErrors Handler

type Handler = ReaderT HandlerEnv Aff Response

type HandlerEnv =
  { accountsAVar :: AVar Accounts
  }