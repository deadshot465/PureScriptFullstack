module Data.Api.QueryUsers where

import Data.Generic.Rep (class Generic)
import Data.UUID (UUID)
import Entity.User (User)
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)
  
newtype QueryUsersRequest = QueryUsersRequest { authToken :: UUID }
derive instance Generic QueryUsersRequest _

instance Encode QueryUsersRequest where
  encode = genericEncode defaultOptions

instance Decode QueryUsersRequest where
  decode = genericDecode defaultOptions

data QueryUsersResultsFailureReason = NotAuthorized | NotAuthenticated
derive instance Generic QueryUsersResultsFailureReason _

instance Encode QueryUsersResultsFailureReason where
  encode = genericEncode defaultOptions

instance Decode QueryUsersResultsFailureReason where
  decode = genericDecode defaultOptions

data QueryUsersResults = QueryUsersResultsSuccess
  { users :: Array User }
  | QueryUsersResultsFailure { reason :: QueryUsersResultsFailureReason }
derive instance Generic QueryUsersResults _

instance Encode QueryUsersResults where
  encode = genericEncode defaultOptions

instance Decode QueryUsersResults where
  decode = genericDecode defaultOptions

newtype QueryUsersResponse = QueryUsersResponse QueryUsersResults
derive instance Generic QueryUsersResponse _

instance Encode QueryUsersResponse where
  encode = genericEncode defaultOptions

instance Decode QueryUsersResponse where
  decode = genericDecode defaultOptions