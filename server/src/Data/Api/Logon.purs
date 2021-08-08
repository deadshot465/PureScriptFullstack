module Data.Api.Logon where

import Data.Generic.Rep (class Generic)
import Data.UUID (UUID)
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)

newtype LogonRequest = LogonRequest
  { username :: String
  , password :: String
  }
derive instance Generic LogonRequest _

instance Encode LogonRequest where
  encode = genericEncode defaultOptions

instance Decode LogonRequest where
  decode = genericDecode defaultOptions

data LogonResults = 
  LogonResultsSuccess
  { authToken :: UUID
  , mustChangePassword :: Boolean
  } | LogonResultsFailure
derive instance Generic LogonResults _

instance Encode LogonResults where
  encode = genericEncode defaultOptions

instance Decode LogonResults where
  decode = genericDecode defaultOptions

newtype LogonResponse = LogonResponse LogonResults
derive instance Generic LogonResponse _

instance Encode LogonResponse where
  encode = genericEncode defaultOptions

instance Decode LogonResponse where
  decode = genericDecode defaultOptions