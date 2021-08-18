module CSS.Missing where

import CSS (AlignContentValue, JustifyContentValue, Value, fromString)

class SpaceEvenly a where
  spaceEvenly :: a

instance SpaceEvenly Value where
  spaceEvenly = fromString "space-evenly"

instance SpaceEvenly AlignContentValue where
  spaceEvenly = fromString "space-evenly"

instance SpaceEvenly JustifyContentValue where
  spaceEvenly = fromString "space-evenly"