module Manager.Account where

import Prelude

import Data.Map (fromFoldable)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Entity.Account (Account(..), Accounts)
import Handler.Account (passwordHashHex)

startup :: Array Account -> Aff (AVar Accounts)
startup accounts = do
  let accountTuples = (\acc@(Account { userName } ) -> Tuple userName acc) <$> accounts
  AVar.new $ fromFoldable accountTuples

shutdown :: AVar Accounts -> Aff Unit
shutdown = void <<< AVar.take

verifyLogon :: AVar Accounts -> String -> String -> Aff (Maybe Account)
verifyLogon accountsAVar userName password = do
  accounts <- AVar.read accountsAVar
  passwordHash' <- passwordHashHex userName password
  let account' = Map.lookup userName accounts
  pure $ (account' >>= \acc@(Account { passwordHash }) -> if passwordHash == passwordHash' then Just acc else Nothing)