module Data.Route where
  
import Prelude hiding ((/))

import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.UUID as UUID
import Data.UserId (UserId(..))
import Routing.Duplex (RouteDuplex', as, optional, path, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = Logon
  | Logoff
  | Users (Maybe String)
  | ChangePassword
  -- | Users (Maybe UserId)

derive instance Generic Route _

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Logon": path "logon" noArgs
  , "Logoff": path "logoff" noArgs
  -- , "Users": "users" / (optional $ userId segment)
  , "Users": "users" / optional segment
  , "ChangePassword": path "change-password" noArgs
  }

userId :: RouteDuplex' String -> RouteDuplex' UserId
userId = as printer parser
  where
    printer (UserId uuid) = UUID.toString uuid
    parser = UUID.parseUUID >>> map UserId >>> note "Invalid UserId"