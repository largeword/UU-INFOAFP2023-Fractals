module ModelAcc where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import GHC.Float (int2Float)

import Data.Array.Accelerate              as A (Matrix, lift, fst, snd, abs, Exp, (*), (-), (+))

-- Separate properties of the screen
screenWidth :: Int
screenWidth = 1000

screenHeight :: Int
screenHeight = 1000

halfScrW :: Int
halfScrW = screenWidth `div` 2

halfScrH :: Int 
halfScrH = screenHeight `div` 2

scaleFactor :: Float
scaleFactor = 1.0 / (0.25 * int2Float screenWidth)


type ZoomScale = Float
type Translation = (Float, Float)

---- Data types regarding the representation and calculation of fractals

-- | A type synonym to define the abstract point calculation function
--  The first argument is the starting point z and the second the complex parameter c
type ZAcc = Exp (Float, Float)
type CAcc = Exp (Float, Float)
type FractalFunctionAcc = ZAcc -> CAcc -> Exp (Float, Float)

-- | Describes which function parameter will be varied in the fractal generation function
data VarParameter = VarZ 
                  | VarC 
                  | VarZandC
                  deriving (Eq)

-- | Contains all information necessary to compute a fractal with a ZFunction
data GeneratorData = GenData
  { position         :: Exp (Float, Float)  -- other name for z in fractal function?
  , offset           :: Exp (Float, Float)  -- offset c in the fractal polynomial
  , escapeRadius     :: Int                 -- TO BE EXPLAINED 
  , parameter        :: VarParameter 
  , func             :: !FractalFunctionAcc -- Strictness required to ensure we do not calculate  
  }                                         -- the same function every iteration

data World = MkWorld 
  { screen         :: Matrix (Float, Float)
  , gData          :: GeneratorData
  , transform      :: (ZoomScale, Translation)
  , currentPicture :: Picture  
  , isChanged      :: Bool
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
  


---- Functions for generating the fractal

-- | Create the fractal characteristic function based on user input
-- First input argument: take absolute of starting point (|Re(z)| + |Im(z)|)^n + c if True 
-- Second: degree n of the polynomial z^n + c 

makeFractalFunctionAcc :: Bool -> Int -> FractalFunctionAcc
makeFractalFunctionAcc isAbs degree            -- do we want decimal degrees? like z^1.5?
  = let  
      polyAcc :: Exp (Float, Float) -> Exp (Float, Float)
      polyAcc point = if isAbs then computePolynomialAcc (A.lift (A.abs (A.fst point), A.abs (A.snd point))) degree 
                               else computePolynomialAcc point degree
      fractalPointAcc :: Exp (Float, Float) -> Exp (Float, Float) -> Exp (Float, Float)
      fractalPointAcc pointZ pointC = A.lift ((A.fst pointZ) A.+ (A.fst pointC), (A.snd pointZ) A.+ (A.snd pointC)) 
    in  
      fractalPointAcc . polyAcc                  
  

-- | compute polynomial values given a complex number z = Re(z) + Im(z),
-- for now we assume that degree n is a positive integer 
computePolynomialAcc :: Exp (Float, Float) -> Int -> Exp (Float, Float)
computePolynomialAcc z 1 = z  
computePolynomialAcc z n | n <= 0    = error ("Non-positive number given as argument: " ++ 
                                              show n ++ ". Please give a positive number") 
                         | even n    = computePolynomialAcc (complexMulAcc z z) (n `div` 2)
                         | otherwise = complexMulAcc z $ computePolynomialAcc z (n-1)


---- UTILITIES ----

complexMulAcc :: Exp (Float, Float) -> Exp (Float, Float) -> Exp (Float, Float)
complexMulAcc point1 point2 = A.lift ( a A.* c A.- b A.* d
                           , a A.* d A.+ b A.* c )
                           where a = A.fst point1
                                 b = A.snd point1
                                 c = A.fst point2
                                 d = A.snd point2


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
    
