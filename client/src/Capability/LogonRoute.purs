module Capability.LogonRoute where

import Prelude

import Halogen (HalogenM, lift)

class Monad m <= LogonRoute m route where
  logonRoute :: PasswordType -> m route

data PasswordType = PasswordPermanent | PasswordTemporary

instance LogonRoute m route => LogonRoute (HalogenM state action slots output m) route where
  logonRoute = lift <<< logonRoute