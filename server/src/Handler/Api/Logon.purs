module Handler.Api.Logon where

import Prelude

import Control.Monad.Except (lift)
import Control.Monad.Reader (ask)
import Data.Api.Logon (LogonRequest(..), LogonResponse(..), LogonResults(..))
import Data.Maybe (Maybe(..))
import Entity.Account (Account(..))
import Foreign.Generic (encodeJSON)
import HTTPure as HTTPure
import Handler.Api.Common (handleApi)
import Handler.Class.ApiHandler (class ApiHandler, Handler)
import Manager.Account (verifyLogon)
import Manager.Session (createSession)

data Logon = Logon

instance ApiHandler Logon where
  handle _ = handleApi handler

handler :: LogonRequest -> Handler
handler (LogonRequest { username, password }) = do
  { accountsAVar, sessionsAVar } <- ask
  verifiedAccount <- lift $ verifyLogon accountsAVar username password
  response <- case verifiedAccount of
    Nothing -> pure $ LogonResponse LogonResultsFailure
    Just (Account { admin, temporaryPassword }) -> do
      authToken <- lift $ createSession sessionsAVar username
      pure $ LogonResponse $ LogonResultsSuccess
                              { authToken
                              , admin
                              , mustChangePassword: temporaryPassword
                              }
  HTTPure.ok $ encodeJSON response