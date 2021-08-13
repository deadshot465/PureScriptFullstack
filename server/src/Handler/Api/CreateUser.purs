module Handler.Api.CreateUser where

import Prelude

import Control.Monad.Except (runExceptT, throwError, withExceptT)
import Control.Monad.Reader (ask, lift)
import Crypto (passwordHashHex)
import Data.Api.CreateUser (CreateUserRequest(..), CreateUserResponse(..), CreateUserResults(..), CreateUserResultsFailureReason(..))
import Data.Either (either, note)
import Entity.Account (Account(..))
import Entity.Session (Session(..))
import Foreign.Generic (encodeJSON)
import HTTPure as HTTPure
import Handler.Account as AH
import Handler.Api.Common (handleApi)
import Handler.Class.ApiHandler (class ApiHandler, Handler)
import Manager.Account as AM
import Manager.Session (verifySession)
import Record (delete, insert)
import Type.Proxy (Proxy(..))
import Util (liftSuccess)
  
data CreateUser = CreateUser

instance ApiHandler CreateUser where
  handle _ = handleApi handler

handler :: CreateUserRequest -> Handler
handler (CreateUserRequest { authToken, user: user' }) = do
  { sessionsAVar, accountsAVar } <- ask
  result <- lift $ runExceptT do
    Session { userName } <- verifySession sessionsAVar authToken <#> note NotAuthenticated # liftSuccess
    Account { admin } <- AM.findAccount accountsAVar userName <#> note NotAuthorized # liftSuccess
    unless admin $ throwError NotAuthorized
    passwordHash <- lift $ passwordHashHex user'.userName user'.password
    let user = delete (Proxy :: _ "password") user'
        account = Account $ insert (Proxy :: _ "passwordHash") passwordHash user
    AM.createAccount accountsAVar account # liftSuccess # (withExceptT $ const AlreadyExists)
    AH.createAccount account # liftSuccess # (withExceptT \(AH.CreateAccountFileError err) -> FileIOError err)
    pure unit
  HTTPure.ok $ encodeJSON $ CreateUserResponse
    $ result # either
      (\reason -> CreateUsersResultsFailure { reason } )
      (const CreateUsersResultsSuccess)
  {- verifiedSession <- lift $ verifySession sessionsAVar authToken
  response <- case verifiedSession of
    Nothing -> CreateUserResponse $ CreateUserResultsFailure { reason: NotAuthenticated }
    Just (Session { userName }) -> do
      admin <- lift $ AM.findAccount accountsAVar userName <#> maybe false (\(Account a) -> a.admin)
      if not admin then CreateUserResponse $ CreateUsersResultsFailure { reason: NotAuthorized }
      else do
        passwordHash <- lift $ passwordHashHex user'.userName user'.password
        let user = delete (Proxy :: _ "password") user'
            account = Account $ insert (Proxy :: _ "passwordHash") passwordHash user
        result <- lift $ AM.createAccount accountsAVar account
        case result of
          AM.CreateAccountAlreadyExists -> CreateUserResponse $ CreateUsersResultsFailure { reason: AlreadyExists }
          AM.CreateAccountSuccess -> do
            result <- lift $ AH.createAccount account
            pure $ case result of
              AH.CreateAccountFileError err -> CreateUserResponse $ CreateUsersResultsFailure { reason: FileIOError err }
              AH.CreateAccountSuccess -> CreateUserResponse $ CreateUsersResultsSuccess -}