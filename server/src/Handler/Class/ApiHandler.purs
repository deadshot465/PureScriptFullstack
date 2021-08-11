module Handler.Class.ApiHandler where

import Data.Either (Either)
import Foreign (MultipleErrors)
import HTTPure (ResponseM)
import Type.Proxy (Proxy)

class ApiHandler :: ∀ k. k -> Constraint
class ApiHandler a where
  handle :: String -> Proxy a -> Either MultipleErrors ResponseM