module Entity.Account where

import Entity.User (UserRow)

newtype Account = Account { | UserRow (passwordHash :: String) }