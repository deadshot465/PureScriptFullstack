module Capability.Navigate where

import Prelude

import Halogen (HalogenM, lift)
  
class Monad m <= Navigate m route where
  navigate :: route -> m Unit

instance Navigate m route => Navigate (HalogenM state action slots output m) route where
  navigate = lift <<< navigate