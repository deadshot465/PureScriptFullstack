module Component.Modal.Common where

import Component.Modal (ButtonDisplay(..), Config, defaultConfig)
  
errorConfig :: Config
errorConfig = defaultConfig { displayButtons = DisplayAffirmative }