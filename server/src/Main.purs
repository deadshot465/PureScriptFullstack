module Main where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Monad.Reader (runReaderT)
import Data.Array (intercalate, last, null, tail)
import Data.Either (Either(..), either, hush)
import Data.Foldable (class Foldable, foldl)
import Data.JSDate (getTime, now, toUTCString)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType (MediaType)
import Data.MediaType.Common as MIME
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Posix.Signal (Signal(..))
import Data.String (Pattern(..), length, split, toLower)
import Data.Tuple (Tuple(..))
import Data.UUID (genUUID)
import Effect (Effect)
import Effect.Aff (launchAff_, try)
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
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Process (onSignal)
import Record (delete)
import Type.Proxy (Proxy(..))

{- router :: Request -> ResponseM
router { path: [ "hello" ], method }
  | method == Get = HTTPure.methodNotAllowed
  | method == Post = HTTPure.ok "Hello"
router { path: [ "goodbye" ] } = HTTPure.ok "Goodbye"
router _ = HTTPure.notFound -}

mimeTypes :: Map String MediaType
mimeTypes = Map.fromFoldable
  [ Tuple "js" MIME.applicationJavascript
  , Tuple "html" MIME.textHTML
  , Tuple "jpg" MIME.imageJPEG
  , Tuple "png" MIME.imagePNG
  , Tuple "map" MIME.textPlain
  ]

mimeType :: String -> Maybe MediaType
mimeType fileName =
  (last =<< (tail $ split (Pattern ".") fileName)) >>= flip Map.lookup mimeTypes <<< toLower

staticRoot :: String
staticRoot = "../client/dist"

staticFiles :: Request -> ResponseM
staticFiles { path } =
  let fileName = if null path then "index.html"
                 else intercalate "/" path in
  mimeType fileName # maybe HTTPure.forbidden \mime -> do
    fileData' <- try $ readTextFile ASCII $ staticRoot <> "/" <> fileName
    fileData' # either (const HTTPure.notFound) \fileData ->
      let headers = HTTPure.headers
            [ Tuple "Content-Length" (show $ length fileData)
            , Tuple "Content-Type" $ unwrap mime
            ] in
      HTTPure.ok' headers fileData

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
router env req@{ body, method }
  | method == HTTPure.Post =
      let handlers = apiHandlers <#> (_ $ body) in
      case hush $ oneOf handlers of
        Nothing -> HTTPure.badRequest body
        Just handler -> runReaderT handler env
  | method == HTTPure.Get = staticFiles req
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