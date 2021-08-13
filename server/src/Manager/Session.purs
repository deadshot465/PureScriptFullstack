module Manager.Session where

import Prelude

import Data.JSDate (getTime, now)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, genUUID)
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Entity.Session (Session(..), Sessions)

shutdown :: AVar Sessions -> Aff Unit
shutdown = void <<< AVar.take

startup :: Aff (AVar Sessions)
startup = AVar.empty

verifySession :: AVar Sessions -> UUID -> Aff (Maybe Session)
verifySession sessionsAVar authToken = do
  expireSessions sessionsAVar
  sessions <- AVar.take sessionsAVar
  nowTime <- getTime <$> liftEffect now
  let Tuple newSessions newSession =
        case Map.lookup authToken sessions of
          Nothing -> Tuple sessions Nothing
          Just (Session session) ->
            let newSession = Session $ session { lastTime = nowTime }
                newSessions = Map.insert authToken newSession sessions in
            Tuple newSessions (Just newSession)
  AVar.put newSessions sessionsAVar
  pure newSession

createSession :: AVar Sessions -> String -> Aff UUID
createSession sessionsAVar userName = do
  sessions <- AVar.take sessionsAVar
  lastTime <- getTime <$> liftEffect now
  authToken <- liftEffect genUUID
  AVar.put (Map.insert authToken (Session { authToken, userName, lastTime }) sessions) sessionsAVar
  pure authToken

sessionTimeout :: Number
sessionTimeout = 4.0 * 60.0 * 60.0 * 1000.0

expireSessions :: AVar Sessions -> Aff Unit
expireSessions sessionsAVar = do
  sessions <- AVar.take sessionsAVar
  nowTime <- getTime <$> liftEffect now
  let newSessions = Map.filter (\(Session { lastTime }) -> nowTime - lastTime > sessionTimeout) sessions
  AVar.put newSessions sessionsAVar