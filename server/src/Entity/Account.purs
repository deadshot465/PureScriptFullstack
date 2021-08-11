module Entity.Acount where

import Entity.User (UserRow)

newtype Account = Account { | UserRow (passwordHash :: String) }