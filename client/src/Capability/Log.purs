module Capability.Log where

import Prelude

import Data.DateTime (DateTime)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Halogen (HalogenM, lift)

data LogLevel = Debug | Info | Warning | Error

derive instance Generic LogLevel _

instance Show LogLevel where
  show = genericShow

type LogEntry =
  { level :: LogLevel
  , message :: String
  , timestamp :: DateTime
  }

class Monad m <= Log m where
  logEntry :: LogLevel -> String -> m LogEntry
  log :: LogEntry -> m Unit

instance Log m => Log (HalogenM state action slots output m) where
  logEntry level = lift <<< logEntry level
  log = lift <<< log