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



-- How to get from a fixed grid of 2-D floating point to adjustable fractals/pictures:
    -- 1.  Initialise world
    -- 2.  record user's panning and zoom (later users can also define their own fractal
    --     parameters so they can make their own fractals)
    -- 3.  Adjust current translation value to sum of current and input values 
    -- 4.  Same for zoom but now by a certain multiplication formula
    -- 5.  Apply translation to each point in the grid
    -- 6.  Apply zoom scaling to each of the resulting points
    -- 7.  Pass the resulting 2D matrix to the generateFractal function: this returns
    --     a 2D array of numeric values.
    -- 8.  Pass this matrix to a colourMatrix function that maps the values to colours.
    -- 9.  Embed each colour in the resulting colourMatrix in a Picture type and combine 
    --     (fold hihi) all those pictures recursively into one picture.
    -- 10. Embed picture in the world and call drawHandler. 
    