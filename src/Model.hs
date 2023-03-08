module Model where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

-- Separate properties of the screen
screenWidth :: Int
screenWidth = 500

screenHeight :: Int
screenHeight = 500

-- Data types regarding the representation and calculation of fractals
data World = MkWorld 
  { screen         :: [[Point]]
  , inputEvents    :: [Event]  
  , zoomScaling    :: Float
  , translation    :: Int
  , currentPicture :: Picture  
}



-- 