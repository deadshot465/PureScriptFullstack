module Data.LoggedOnUser where

import Data.UUID (UUID)

type LoggedOnUser =
  { authToken :: UUID
  , admin :: Boolean
  }