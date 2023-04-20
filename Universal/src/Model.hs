module Model where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import GHC.Float (int2Float)


-- | Dimensional properties of the screen
screenWidth :: Int
screenWidth = 1000

screenHeight :: Int
screenHeight = 1000

halfScrW :: Int
halfScrW = screenWidth `div` 2

halfScrH :: Int
halfScrH = screenHeight `div` 2

-- | making the scale factor dependant on the smallest of screenWidth and screenHeight
--   ensures the default scale always shows the whole fractal
scaleFactor :: Float
scaleFactor = 1.0 / (0.25 * int2Float (min screenWidth screenHeight))


---- Data types regarding the representation and calculation of fractals

-- | A type synonym to define the abstract point calculation function
--  The first argument is the starting point z and the second the complex parameter c
type Z = Point
type C = Point

type FractalFunction = Z -> C -> Point

-- | Describes which function parameter will be varied in the fractal generation function
data VarParameter = VarZ
                  | VarC

-- | Contains all information necessary to compute a fractal with a ZFunction
data GeneratorData = GenData
  { position         :: Point             -- other name for z in fractal function
  , offset           :: Point             -- offset c in the fractal polynomial
  , escapeRadius     :: Int               -- The amount of iterations we perform
  , parameter        :: VarParameter      -- Describes which function parameterr will be varied
  , func             :: !FractalFunction  -- Strictness required to ensure we do not calculate
  }                                       -- the same function every iteration

-- | Simple type synonyms to describe transformation: Zooming and translation
type ZoomScale = Float
type Translation = (Float, Float)

-- | Contains all information of the world state
data World = MkWorld
  { screen         :: Grid Point                -- The grid with all screen values
  , gData          :: GeneratorData             -- The generatorData with which to compute the fractal
  , transform      :: (ZoomScale, Translation)  -- The transformation values
  , currentPicture :: Picture                   -- A cached version of our current picture
  , isChanged      :: Bool                      -- Flags whether we need to recompute
  }


-- | Datatypes describing events to change the scene
--   Initially these were designed to be easily recorded in a list
--   This would allow certain actions to happen every step
--   Since this feature was scrapped, these are more-or-less vestigial
data Zoom = In
          | Out

data Direction = Left'
               | Right'
               | Up'
               | Down'

data EventAction = Move Direction
                 | Zoom Zoom


---- Functions for generating the fractal

-- | Create the fractal characteristic function based on user input
--   First input argument: take absolute of starting point (|Re(z)| + |Im(z)|)^n + c if True
--   Second: degree n of the polynomial z^n + c
--   Polynomial degrees <= 0 or non-integer polynomial degrees are not supported
makeFractalFunction :: Bool -> Int -> FractalFunction
makeFractalFunction isAbs degree =
  let
      poly (zReal, zImag) = if isAbs 
                            then computePolynomial (abs zReal, abs zImag) degree
                            else computePolynomial (zReal, zImag) degree
      fractalPoint (zReal', zImag') (cReal, cImag) = (zReal' + cReal, zImag' + cImag)
  in fractalPoint . poly

-- | Compute polynomial values given a complex number z = Re(z) + Im(z),
--   We assume that degree n is a positive integer
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
