module Data.Api.CreateUser where

import Data.Generic.Rep (class Generic)
import Data.UUID (UUID)
import Entity.User (UserRow)
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)

newtype CreateUserRequest = CreateUserRequest
  { authToken :: UUID
  , user :: Record (UserRow (password :: String))
  }
derive instance Generic CreateUserRequest _

instance Encode CreateUserRequest where
  encode = genericEncode defaultOptions

instance Decode CreateUserRequest where
  decode = genericDecode defaultOptions

data CreateUserResultsFailureReason = AlreadyExists | NotAuthorized | NotAuthenticated
derive instance Generic CreateUserResultsFailureReason _

instance Encode CreateUserResultsFailureReason where
  encode = genericEncode defaultOptions

instance Decode CreateUserResultsFailureReason where
  decode = genericDecode defaultOptions

data CreateUserResults = CreateUsersResultsSuccess
  | CreateUsersResultsFailure { reason :: CreateUserResultsFailureReason }
derive instance Generic CreateUserResults _

instance Encode CreateUserResults where
  encode = genericEncode defaultOptions

instance Decode CreateUserResults where
  decode = genericDecode defaultOptions

newtype CreateUserResponse = CreateUserResponse CreateUserResults
derive instance Generic CreateUserResponse _

instance Encode CreateUserResponse where
  encode = genericEncode defaultOptions

instance Decode CreateUserResponse where
  decode = genericDecode defaultOptions