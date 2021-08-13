module Main where

import Prelude

import Control.Monad.Reader (runReaderT)
import Data.Array (head)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (oneOf, (:|))
import Data.Posix.Signal (Signal(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (error, log)
import HTTPure (Request, ResponseM)
import HTTPure as HTTPure
import Handler.Account as AccountHandler
import Handler.Api.Logon (Logon)
import Handler.Class.ApiHandler (HandlerEnv, handle)
import Manager.Account as AccountManager
import Node.Process (onSignal)
import Type.Proxy (Proxy(..))

{- router :: Request -> ResponseM
router { path: [ "hello" ], method }
  | method == Get = HTTPure.methodNotAllowed
  | method == Post = HTTPure.ok "Hello"
router { path: [ "goodbye" ] } = HTTPure.ok "Goodbye"
router _ = HTTPure.notFound -}

router :: HandlerEnv -> Request -> ResponseM
router env { body, method }
  | method == HTTPure.Post =
      case hush =<< (head $ oneOf $ (handle body (Proxy :: _ Logon)) :| []) of
        Nothing -> HTTPure.badRequest body
        Just handler -> runReaderT handler env
  | otherwise = HTTPure.methodNotAllowed

port :: Int
port = 3000

main :: Effect Unit
main = launchAff_ do
  loadResults <- AccountHandler.loadAccounts
  case loadResults of
    Left err -> error $ "Cannot load accounts: " <> show err
    Right accounts -> do
      accountsAVar <- AccountManager.startup accounts
      liftEffect do
        shutdown <- HTTPure.serve port (router { accountsAVar }) $ log $ "Server up running on port: " <> show port
        let shutdownServer = launchAff_ do
              log "Shutting down server..."
              AccountManager.shutdown accountsAVar
              liftEffect $ shutdown $ log "Server shutdown."
        onSignal SIGINT shutdownServer
        onSignal SIGTERM shutdownServer