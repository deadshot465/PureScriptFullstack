module Crypto where

import Prelude

import Data.Char (toCharCode)
import Data.Foldable (foldl)
import Data.String (length)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Crypto.Hash (Algorithm(..), hex)
import Random.LCG (Seed, mkSeed)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (sample)

userNameSeed :: String -> Seed
userNameSeed name = mkSeed $ foldl (*) 1 $ toCharCode <$> toCharArray name

userNameSalt :: String -> Int -> String
userNameSalt name length = fromCharArray $ sample (userNameSeed name) length arbitrary

passwordHashHex :: String -> String -> Aff String
passwordHashHex userName password =
  let salt = userNameSalt userName (3 * length userName) in
  liftEffect $ hex SHA512 (password <> salt)