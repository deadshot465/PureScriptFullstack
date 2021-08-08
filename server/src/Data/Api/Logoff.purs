module Data.Api.Logoff where

import Data.Generic.Rep (class Generic)
import Data.UUID (UUID)
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)

newtype LogoffRequest = LogoffRequest { authToken :: UUID }
derive instance Generic LogoffRequest _

instance Encode LogoffRequest where
  encode = genericEncode defaultOptions

instance Decode LogoffRequest where
  decode = genericDecode defaultOptions

data LogoffResults = LogoffResultsSuccess | LogoffResultsFailure
derive instance Generic LogoffResults _

instance Encode LogoffResults where
  encode = genericEncode defaultOptions

instance Decode LogoffResults where
  decode = genericDecode defaultOptions

newtype LogoffResponse = LogoffResponse LogoffResults
derive instance Generic LogoffResponse _

instance Encode LogoffResponse where
  encode = genericEncode defaultOptions

instance Decode LogoffResponse where
  decode = genericDecode defaultOptions