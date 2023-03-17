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

-- | Describes what kind of fractal will be generated
data Fractal = MBrot              
             | Julia
            --  | BurnedShip 
            --  | Newton/Nova 
-- what would happen if we change the degree of the Mandelbrot function?

-- | Contains all information necessary to compute a fractal with a ZFunction
data GeneratorData = GenData
  { position         :: Point            -- other name for z in fractal function?
  , escapeRadius     :: Int              -- other name for c in fractal function?
  , cplexParam       :: Point
  , fractalType      :: Fractal        
  , degree           :: Int              -- degree n of polynomial ZFunction
  -- , hasConstPosition :: Bool             -- if True then escapeRadius will be varied
                                         -- by the ZFunction, otherwise its position
                                         -- (then the escapeRadius will be kept constant)
                                         -- something that is only necessary when dealing
                                         -- with Nova fractals  
  }
  -- do we want decimal degrees? like z^1.5?

data World = MkWorld 
  { screen         :: Grid Point
  , gData          :: GeneratorData
  , inputEvents    :: [Event]  
  , zoomScaling    :: Float
  , translation    :: (Int, Int)
  , currentPicture :: Picture  
  , isChanged      :: Bool
  }

startWorld :: World
startWorld = MkWorld
    [[(fromIntegral $ x - halfScrW, fromIntegral $ y - halfScrH) | x <- [0..screenWidth-1]] | y <- [0..screenHeight-1]]
    (GenData { position = (0,0), escapeRadius = 100, cplexParam = (0,0), fractalType = MBrot, degree = 2 })
    []
    1
    (0, 0)
    Blank
    True

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
    
