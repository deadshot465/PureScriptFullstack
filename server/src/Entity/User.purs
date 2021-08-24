module Entity.User where
  
import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)

type UserRow r =
  ( userName :: String
  , temporaryPassword :: Boolean
  , admin :: Boolean
  , firstName :: String
  , lastName :: String
  | r
  )

newtype User = User { | UserRow () }

derive instance Generic User _
derive instance Newtype User _
derive instance Eq User

instance Encode User where
  encode = genericEncode defaultOptions

instance Decode User where
  decode = genericDecode defaultOptions