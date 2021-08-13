module Handler.Account where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Bifunctor (lmap)
import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Newtype (unwrap)
import Data.String (length)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.Utils (lines)
import Data.Traversable (intercalate, sequence)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Entity.Account (Account(..))
import Node.Crypto.Hash (Algorithm(..), hex)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (appendTextFile, exists, readTextFile, writeTextFile)
import Parser.Account (accountParser)
import Random.LCG (Seed, mkSeed)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (sample)
import Text.Parsing.Parser (ParseError, runParserT)

accountsFile :: String  
accountsFile = "accounts.csv"

bootstrapAccount :: Aff String
bootstrapAccount = do
  let userName = "admin"
      password = "admin"
  passwordHash <- passwordHashHex userName password
  let true' = show true
  pure $ intercalate "," [ userName, passwordHash, true', true', "Joe", "Admin" ]

loadAccounts :: Aff (Either ParseError (Array Account))
loadAccounts = do
  exists' <- try $ exists accountsFile
  let exists = case exists' of
                Left _ -> false
                Right b -> b
  unless exists do
    bsa <- bootstrapAccount
    writeTextFile ASCII accountsFile bsa
  accountLines <- lines <$> readTextFile ASCII accountsFile
  pure $ sequence $ unwrap <<< flip runParserT accountParser <$> accountLines

accountToCSV :: Account -> String
accountToCSV (Account { userName, passwordHash, temporaryPassword, admin, firstName, lastName }) =
  intercalate "," [ userName, passwordHash, show temporaryPassword, show admin, firstName, lastName ]

createAccount :: Account -> Aff (Either String Unit)
createAccount account = lmap show <$> (try $ appendTextFile ASCII accountsFile $ accountToCSV account)

userNameSeed :: String -> Seed
userNameSeed name = mkSeed $ foldl (*) 1 $ toCharCode <$> toCharArray name

userNameSalt :: String -> Int -> String
userNameSalt name length = fromCharArray $ sample (userNameSeed name) length arbitrary

passwordHashHex :: String -> String -> Aff String
passwordHashHex userName password =
  let salt = userNameSalt userName (3 * length userName) in
  liftEffect $ hex SHA512 (password <> salt)