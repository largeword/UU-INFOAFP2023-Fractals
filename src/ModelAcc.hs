{-# LANGUAGE TypeOperators #-}

module ModelAcc where

import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Interface.IO.Interact
import GHC.Float (int2Float)

import Data.Array.Accelerate              as A
import Prelude as P

-- Separate properties of the screen
screenWidth :: Int
screenWidth = 2000

screenHeight :: Int
screenHeight = 2000

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
                  deriving (P.Eq)

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
  deriving (P.Eq, Show)

data Direction = Left'
               | Right'
               | Up'
               | Down'
  deriving (P.Eq, Show)

data EventAction = Move Direction
                 | Zoom Zoom
  deriving (P.Eq, Show)
  
-- | Arguments: Red Green Blue Alpha (all values should be in [0..1])
type ColorAcc = (Float, Float, Float, Float)
type Cubic = Array DIM3

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
computePolynomialAcc z n | n P.<= 0    = error ("Non-positive number given as argument: " P.++ 
                                              show n P.++ ". Please give a positive number") 
                         | P.even n    = computePolynomialAcc (complexMulAcc z z) (n `div` 2)
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
gridMap f = P.map (P.map f)

-- | Default color list
--   https://colorswall.com/palette/128774
colorList :: A.Vector ColorAcc
colorList = fromList dim $ P.map toFloats rgbs
  where  --    R    G    B   A
    rgbs = [ (43.0 , 192.0, 232.0, 255.0)
           , (246.0, 203.0, 102.0, 255.0)
           , (72.0 , 68.0 , 152.0, 255.0)
           , (99.0 , 167.0, 94.0 , 255.0)
           , (160.0, 172.0, 180.0, 255.0)
           , (68.0 , 62.0 , 94.0 , 255.0)]
    toFloats (a, b, c, d) = (a / 255, b / 255, c / 255, d / 255)
    dim :: Z :. Int
    dim = Z :. 6

