module Entity.Session where

import Data.Map (Map)
import Data.UUID (UUID)
  
newtype Session = Session
  { authToken :: UUID
  , userName :: String
  , lastTime :: Number
  }

type Sessions = Map UUID Session