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

---- Data types regarding the representation and calculation of fractals

-- | A type synonym to define the abstract point calculation function
--  The first argument is the starting point z and the second the complex parameter c
type Z = Point
type C = Point

type FractalFunction = Z -> C -> Point  

-- | Describes which function parameter will be varied in the fractal generation function
data VarParameter = VarZ 
                  | VarC 
                  | VarZandC

-- | Contains all information necessary to compute a fractal with a ZFunction
data GeneratorData = GenData
  { position         :: Point            -- other name for z in fractal function?
  , offset           :: Point            -- offset c in the fractal polynomial
  , escapeRadius     :: Int              -- TO BE EXPLAINED 
  , parameter        :: VarParameter 
  , func             :: FractalFunction       
  }

data World = MkWorld 
  { screen         :: Grid Point
  , gData          :: GeneratorData
  , inputEvents    :: [Event]  
  , zoomScaling    :: Float
  , translation    :: (Int, Int)
  , currentPicture :: Picture  
  , isChanged      :: Bool
  }

  


---- Functions for generating the fractal

-- | Create the fractal characteristic function based on user input
-- First input argument: take absolute of starting point (|Re(z)| + |Im(z)|)^n + c if True 
-- Second: degree n of the polynomial z^n + c 

makeFractalFunction :: Bool -> Int -> FractalFunction
makeFractalFunction isAbs degree            -- do we want decimal degrees? like z^1.5?
  = let  
      -- poly :: Point -> Point    
      poly (zReal, zImag) = if isAbs then computePolynomial (abs zReal, abs zImag) degree 
                                     else computePolynomial (zReal, zImag) degree 
      -- fractalpoint :: Point -> Point -> Point
      fractalPoint (zReal', zImag') (cReal, cImag) = (zReal' + cReal, zImag' + cImag) 
    in  
      fractalPoint . poly                  
  

-- | compute polynomial values given a complex number z = Re(z) + Im(z),
-- for now we assume that degree n is a positive integer 
computePolynomial :: Point -> Int -> Point
computePolynomial z 1 = z  
computePolynomial z n | n <= 0    = error ("Non-positive number given as argument: " ++ 
                                           show n ++ ". Please give a positive number") 
                      | even n    = computePolynomial (complexMul z z) (n `div` 2)
                      | otherwise = complexMul z $ computePolynomial z (n-1)


---- UTILITIES ----

complexMul :: Point -> Point -> Point
complexMul (a, b) (c, d) = ( a * c - b * d
                           , a * d + b * c )

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
    
