module AppTheme where

import Prelude

import CSS (CSS, fontFamily, rgba, sansSerif, toRGBA)
import Color (Color, rgb)
import Data.NonEmpty ((:|))
  
paperColor :: Color
paperColor = rgb 0xd9 0xd9 0xd9

themeColor :: Color
themeColor = rgb 0x00 0x66 0x75

themeFont :: CSS
themeFont = fontFamily [ "Verdana" ] $ sansSerif :| []

selectedColor :: Color
selectedColor = themeColor # toRGBA # \{ r, g, b } -> rgba r g b 0.75