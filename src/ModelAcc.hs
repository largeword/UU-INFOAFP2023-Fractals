module ModelAcc (hiding Z) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import GHC.Float (int2Float)

import Data.Array.Accelerate

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
newtype Z = Point
newtype C = Point

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
  , func             :: !FractalFunction -- Strictness required to ensure we do not calculate  
  }                                      -- the same function every iteration

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
  
-- | Arguments: Red Green Blue Alpha (all values should be in [0..1])
type ColorAcc = (Float, Float, Float, Float)
type Cubic = Array DIM3

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
colorList :: Vector ColorAcc
colorList = fromList (Z:.6) $ map toFloats rbgs
  where  --    R    G    B   A
    rgbs = [ (43.0 , 192.0, 232.0, 255.0)
           , (246.0, 203.0, 102.0, 255.0)
           , (72.0 , 68.0 , 152.0, 255.0)
           , (99.0 , 167.0, 94.0 , 255.0)
           , (160.0, 172.0, 180.0, 255.0)
           , (68.0 , 62.0 , 94.0 , 255.0)]
    toFloats (a, b, c, d) = (a / 255, b / 255, c / 255, d / 255)

