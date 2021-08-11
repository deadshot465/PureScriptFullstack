module Handler.Api.Logon where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Api.Logon (LogonRequest)
import Foreign.Generic (decodeJSON)
import HTTPure (ResponseM)
import HTTPure as HTTPure
import Handler.Class.ApiHandler (class ApiHandler)

data Logon = Logon

instance ApiHandler Logon where
  handle request _ = do
    logonReq <- runExcept (decodeJSON request :: _ LogonRequest)
    pure $ handler logonReq

handler :: LogonRequest -> ResponseM
handler _ = do
  HTTPure.notFound