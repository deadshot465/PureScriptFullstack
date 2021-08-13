module Handler.Account where

import Prelude

import Control.Monad.Error.Class (try)
import Crypto (passwordHashHex)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Traversable (intercalate)
import Effect.Aff (Aff)
import Entity.Account (Account(..))
import Node.Encoding (Encoding(..))
import Node.FS.Aff (appendTextFile, exists, readTextFile, writeTextFile)
import Parser.Account (accountsParser)
import Text.Parsing.Parser (ParseError, runParserT)

data CreateAccountError = CreateAccountFileError String

accountsFile :: String  
accountsFile = "accounts.csv"

bootstrapAccount :: Aff String
bootstrapAccount = do
  let userName = "admin"
      password = "admin"
  passwordHash <- passwordHashHex userName password
  pure $ accountToCSV $ Account
    { userName
    , passwordHash
    , temporaryPassword: true
    , admin: true
    , firstName: "Joe"
    , lastName: "Admin"
    }

loadAccounts :: Aff (Either ParseError (Array Account))
loadAccounts = do
  exists' <- try $ exists accountsFile
  let exists = case exists' of
                Left _ -> false
                Right b -> b
  unless exists do
    bsa <- bootstrapAccount
    writeTextFile ASCII accountsFile bsa
  fileData <- readTextFile ASCII accountsFile
  pure $ unwrap $ runParserT fileData accountsParser
  --accountLines <- lines <$> readTextFile ASCII accountsFile
  --pure $ sequence $ unwrap <<< flip runParserT accountParser <$> accountLines

accountToCSV :: Account -> String
accountToCSV (Account { userName, passwordHash, temporaryPassword, admin, firstName, lastName }) =
  intercalate ","
    [ userName
    , passwordHash
    , show temporaryPassword
    , show admin
    , firstName
    , lastName
    ] <> "\n"

createAccount :: Account -> Aff (Either CreateAccountError Unit)
createAccount account = lmap show
  <$> (try $ appendTextFile ASCII accountsFile $ accountToCSV account)
  <#> case _ of
    Left err -> Left $ CreateAccountFileError err
    Right _ -> Right unit