module Model where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

-- Separate properties of the screen
screenWidth :: Int
screenWidth = 500

screenHeight :: Int
screenHeight = 500

halfScrW :: Int
halfScrW = screenWidth `div` 2

halfScrH :: Int
halfScrH = screenHeight `div` 2


-- Data types regarding the representation and calculation of fractals
data World = MkWorld 
  { screen         :: [[Point]]
  , inputEvents    :: [Event]  
  , zoomScaling    :: Float
  , translation    :: (Int, Int)
  , currentPicture :: Picture  
  , isChanged      :: Bool
}


startWorld :: World
startWorld = MkWorld
    [[(fromIntegral $ x - halfScrW, fromIntegral $ y - halfScrH) | x <- [0..screenWidth-1]] | y <- [0..screenHeight-1]]
    []
    1
    (0, 0)
    Blank
    True
