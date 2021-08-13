module Main where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Monad.Reader (runReaderT)
import Data.Either (Either(..), hush)
import Data.Foldable (class Foldable, foldl)
import Data.JSDate (getTime, now, toUTCString)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Posix.Signal (Signal(..))
import Data.UUID (genUUID)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (error, log)
import Foreign (MultipleErrors)
import HTTPure (Request, ResponseM)
import HTTPure as HTTPure
import Handler.Account as AccountHandler
import Handler.Api.CreateUser (CreateUser)
import Handler.Api.Logoff (Logoff)
import Handler.Api.Logon (Logon)
import Handler.Api.QueryUsers (QueryUsers)
import Handler.Class.ApiHandler (HandlerEnv, Handler, handle)
import Manager.Account as AccountManager
import Manager.Session as SessionManager
import Node.Process (onSignal)
import Record (delete)
import Type.Proxy (Proxy(..))

{- router :: Request -> ResponseM
router { path: [ "hello" ], method }
  | method == Get = HTTPure.methodNotAllowed
  | method == Post = HTTPure.ok "Hello"
router { path: [ "goodbye" ] } = HTTPure.ok "Goodbye"
router _ = HTTPure.notFound -}

oneOf :: ∀ f t a. Foldable f => Alt t => NonEmpty f (t a) -> t a
oneOf (x :| xs) = foldl (<|>) x xs

apiHandlers :: NonEmpty Array (String -> Either MultipleErrors Handler)
apiHandlers =
  handle (Proxy :: _ Logon) :| [ 
    handle (Proxy :: _ Logoff)
  , handle (Proxy :: _ CreateUser)
  , handle (Proxy :: _ QueryUsers)
  ]

router :: HandlerEnv -> Request -> ResponseM
router env { body, method }
  | method == HTTPure.Post =
      let handlers = apiHandlers <#> (_ $ body) in
      case hush $ oneOf handlers of
        Nothing -> HTTPure.badRequest body
        Just handler -> runReaderT handler env
  | otherwise = HTTPure.methodNotAllowed

loggingRouter :: HandlerEnv -> Request -> ResponseM
loggingRouter env req = do
  start <- liftEffect now
  id <- liftEffect genUUID
  let idStr = " (" <> show id <> ")"
      ts dt = "(" <> toUTCString dt <> ") "
  log $ ts start <> "REQUEST: " <> show req <> idStr
  res <- router env req
  end <- liftEffect now
  let duration = getTime end - getTime start
  log $ ts end <> "RESPONSE: "
    <> (show $ delete (Proxy :: _ "writeBody") res)
    <> " [" <> show duration <> " ms]"
    <> idStr
  pure res

port :: Int
port = 3000

main :: Effect Unit
main = launchAff_ do
  loadResults <- AccountHandler.loadAccounts
  case loadResults of
    Left err -> error $ "Cannot load accounts: " <> show err
    Right accounts -> do
      accountsAVar <- AccountManager.startup accounts
      sessionsAVar <- SessionManager.startup
      liftEffect do
        shutdown <- HTTPure.serve port (loggingRouter { accountsAVar, sessionsAVar }) $ log $ "Server up running on port: " <> show port
        let shutdownServer = launchAff_ do
              log "Shutting down server..."
              SessionManager.shutdown sessionsAVar
              AccountManager.shutdown accountsAVar
              liftEffect $ shutdown $ log "Server shutdown."
        onSignal SIGINT shutdownServer
        onSignal SIGTERM shutdownServer