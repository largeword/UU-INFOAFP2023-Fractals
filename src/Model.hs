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
  { screen         :: Grid Point
  , gData          :: GeneratorData
  , inputEvents    :: [EventAction]  
  , zoomScaling    :: Float
  , translation    :: (Float, Float)
  , currentPicture :: Picture  
  , isChanged      :: Bool
  }

startWorld :: World
startWorld = MkWorld
    [[(fromIntegral $ x - halfScrW, fromIntegral $ y - halfScrH) | x <- [0..screenWidth-1]] | y <- [0..screenHeight-1]]
    (GenData (0,0))
    []
    (1 / 125)
    (0, 0)
    Blank
    True


data GeneratorData = GenData
  { position :: (Float, Float)

  }





data Zoom = In
          | Out
  deriving (Eq, Show)

data Direction = Left'
               | Right'
               | Up'
               | Down'
  deriving (Eq, Show)

data EventAction = Move Direction
                 | Zoom Zoom
  deriving (Eq, Show)

type Grid a = [[a]]

gridMap :: (a -> b) -> Grid a -> Grid b
gridMap f = map (map f)



-- | Default color list
--   https://colorswall.com/palette/128774
colorList :: [Color]
colorList = [makeColorI r g b a | (r,g,b,a) <- rgbs ]
  where  --    R    G    B   A
    rgbs = [ (43 , 192, 232, 255)
           , (246, 203, 102, 255)
           , (72 , 68 , 152, 255)
           , (99 , 167, 94 , 255)
           , (160, 172, 180, 255)
           , (68 , 62 , 94 , 255)]

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
    
