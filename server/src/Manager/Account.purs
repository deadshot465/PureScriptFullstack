module Manager.Account where

import Prelude

import Crypto (passwordHashHex)
import Data.Either (Either(..))
import Data.Map (fromFoldable)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Entity.Account (Account(..), Accounts)
import Util (withAVar)

data CreateAccountError = CreateAccountAlreadyExists

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

createAccount :: AVar Accounts -> Account -> Aff (Either CreateAccountError Unit)
createAccount accountsAVar account@(Account { userName }) = do
  withAVar accountsAVar
    \accounts -> pure $
        if Map.member userName accounts then
          Tuple accounts (Left CreateAccountAlreadyExists)
        else do
          Tuple (Map.insert userName account accounts) (Right unit)

findAccount :: AVar Accounts -> String -> Aff (Maybe Account)
findAccount accountsAVar userName = do
  AVar.read accountsAVar >>= pure <<< Map.lookup userName