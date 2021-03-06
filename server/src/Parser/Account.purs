module Parser.Account where

import Prelude

import Control.Alt ((<|>))
import Data.Array (catMaybes, fromFoldable, many, some, (:))
import Data.CodePoint.Unicode (isAlpha, isAlphaNum, isLower, isUpper)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.String (codePointFromChar)
import Data.String.CodeUnits (fromCharArray)
import Entity.Account (Account(..))
import Text.Parsing.Parser (ParserT, fail)
import Text.Parsing.Parser.Combinators (notFollowedBy, sepBy)
import Text.Parsing.Parser.String (anyChar, char, satisfy, string)

type AccountParserT a = ParserT String Identity a

hex :: AccountParserT String
hex = fromCharArray <$> (some $ satisfy isHex)
  where
    isHex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')

passwordHash :: AccountParserT String
passwordHash = hex

boolean :: AccountParserT Boolean
{- boolean = do
  bool <- fromCharArray <$> some (satisfy (isAlpha <<< codePointFromChar))
  case bool of
    "true" -> pure true
    "false" -> pure false
    _ -> fail "Invalid boolean" -}
boolean = (string "true" *> pure true) <|> (string "false" *> pure false) <|> fail "Invalid Boolean"

temporaryPassword :: AccountParserT Boolean
temporaryPassword = boolean

admin :: AccountParserT Boolean
admin = boolean

properName :: AccountParserT String
properName = do
  first <- satisfy (isUpper <<< codePointFromChar)
  rest <- many $ satisfy (isLower <<< codePointFromChar)
  pure $ fromCharArray $ first : rest

firstName :: AccountParserT String
firstName = properName

lastName :: AccountParserT String
lastName = properName

userName :: AccountParserT String
userName = do
  alpha <- satisfy (isAlpha <<< codePointFromChar)
  alphaNums <- many $ satisfy (isAlphaNum <<< codePointFromChar)
  pure $ fromCharArray $ alpha : alphaNums

accountParser :: AccountParserT Account
accountParser = do
  userName' <- userName # comma
  passwordHash' <- passwordHash # comma
  temporaryPassword' <- temporaryPassword # comma
  admin' <- admin # comma
  firstName' <- firstName # comma
  lastName' <- lastName
  pure $ Account
    { userName: userName'
    , passwordHash: passwordHash'
    , temporaryPassword: temporaryPassword'
    , admin: admin'
    , firstName: firstName'
    , lastName: lastName'
    }
  where
    comma :: ??? a. AccountParserT a -> AccountParserT a
    comma p = p <* char ','

accountsParser :: AccountParserT (Array Account)
accountsParser =
  ((Just <$> accountParser <|> pure Nothing)
  `sepBy` char '\n' <#> catMaybes <<< fromFoldable)
  <* notFollowedBy anyChar <|> fail "Failed to parse complete file."