module AppM where

import Prelude

import Capability.Log (class Log)
import Capability.LogonRoute (class LogonRoute, PasswordType(..))
import Capability.Navigate (class Navigate)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Data.Maybe (Maybe(..))
import Data.Route (Route)
import Data.Route as Route
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Now (nowDateTime)
import Env (Env)
import Routing.Duplex as RouteDuplex
import Routing.Hash (setHash)

newtype AppM a = AppM (ReaderT Env Aff a)

derive newtype instance Functor AppM
derive newtype instance Apply AppM
derive newtype instance Applicative AppM
derive newtype instance Bind AppM
derive newtype instance Monad AppM
derive newtype instance MonadEffect AppM
derive newtype instance MonadAff AppM
derive newtype instance MonadAsk Env AppM

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM reader) = runReaderT reader env

instance LogonRoute AppM Route where
  logonRoute PasswordPermanent = pure $ Route.Users Nothing
  logonRoute PasswordTemporary = pure Route.ChangePassword

instance Navigate AppM Route where
  navigate = liftEffect <<< setHash <<< RouteDuplex.print Route.routeCodec

instance Log AppM where
  logEntry level message = liftEffect nowDateTime >>= pure <<< { level, message, timestamp: _ }
  log = Console.log <<< show