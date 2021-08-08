module Main where

import Prelude

import Data.Posix.Signal (Signal(..))
import Effect (Effect)
import Effect.Class.Console (log)
import HTTPure (Request, ResponseM)
import HTTPure as HTTPure
import Node.Process (onSignal)

{- router :: Request -> ResponseM
router { path: [ "hello" ], method }
  | method == Get = HTTPure.methodNotAllowed
  | method == Post = HTTPure.ok "Hello"
router { path: [ "goodbye" ] } = HTTPure.ok "Goodbye"
router _ = HTTPure.notFound -}

router :: Request -> ResponseM
router _ = HTTPure.notFound

port :: Int
port = 3000

main :: Effect Unit
main = do
  shutdown <- HTTPure.serve port router $ log $ "Server up running on port: " <> show port
  let shutdownServer = do
        log "Shutting down server..."
        shutdown $ log "Server shutdown."
  onSignal SIGINT shutdownServer
  onSignal SIGTERM shutdownServer
  pure unit