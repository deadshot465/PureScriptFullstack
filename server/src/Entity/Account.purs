module Entity.Account where

import Data.Map (Map)
import Entity.User (UserRow)

newtype Account = Account { | UserRow (passwordHash :: String) }

type Accounts = Map String Account