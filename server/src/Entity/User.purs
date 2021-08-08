module Entity.User where
  
import Data.Generic.Rep (class Generic)
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

instance Encode User where
  encode = genericEncode defaultOptions

instance Decode User where
  decode = genericDecode defaultOptions