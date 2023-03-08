module View where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact


drawHandler :: World -> Picture
drawHandler _ = Color red (Circle 25) 
