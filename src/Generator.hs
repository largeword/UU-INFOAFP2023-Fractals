module Generator (getSequences, getEscapeSteps) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.List
import Data.List.Split
import Data.Function

import Model


-- | A type synonym to define the abstract point calculation function
--   Hopefully, we may be able to allow users to define their own version of this
--   The first argument is the z, the second is the c
type ZFunction = Point -> Point -> Point


-- | Default implementation of the fractal formula
--   Defines the function z' = z^2 + c
nextZDefault :: ZFunction
nextZDefault (zr, zi) (cr, ci) = let (z2r, z2i) = ( zr ** 2 - zi ** 2    -- real
                                                  , 2 * zr * zi       )  -- imag
                                  in ( z2r + cr    -- real
                                     , z2i + ci )  -- imag


-- | Generates a list of length n, defining the iterations
--   according to the ZFunction provided
getIterations :: ZFunction -> (Point, Point) -> [Point]
getIterations zf (z, c) = iterate (`zf` c) z



------ Temporary ----------------

-- generate iterations of a fixed-z
fixedC :: GeneratorData -> Point -> [Point]
fixedC d z = getIterations nextZDefault (z, c)
  where
      c = position d

-- generate iterations of a fixed-c
fixedZ :: Point -> [Point]
fixedZ c = getIterations nextZDefault (z, c)
  where
      z = (0, 0)

----------------------------------


-- | Given a scaled grid, compute the sequences of iterations
getSequences :: GeneratorData -> Grid Point -> Grid [Point]
getSequences d grid = gridMap (fixedC d) grid


-- | Given a sequenced grid and a step treshold n,
--   compute the steps in which the point escapes the treshold
getEscapeSteps :: Int -> Grid [Point] -> Grid Int
getEscapeSteps n grid = gridMap ( length              -- the amount of unescaped values
                                . filter (== True)    -- discard all escaped values
                                . map crossThreshold  -- convert the sequence to bools
                                . take n              -- limit the size of the sequence
                                ) grid                -- the sequenced grid 
  where
    -- | Given a point, calculate whether it's close enough to the fractal interior
    --   zx or zy can also be either NaN or Infinity
    crossThreshold :: Point -> Bool
    crossThreshold (zx, zy) = if (isNaN zx) || (isNaN zy) then False
                              else (zx ** 2 + zy ** 2) < 100







---------- FATE OF CODE TBD --------------

-- -- generate possition for each pixel
-- getPixelPoint :: Int -> Int -> Point -> Point -> [Point]
-- getPixelPoint xPixels yPixels (xMin, yMin) (xMax, yMax) = map (,) [xMin, xMin + xGap .. xMax] <*> [yMin, yMin + yGap .. yMax]
--                                                           where xGap = (xMax - xMin) / fromIntegral (xPixels - 1)
--                                                                 yGap = (yMax - yMin) / fromIntegral (yPixels - 1)

-- -- reshape list/array
-- reshapeArray :: [a] -> Int -> [[a]]
-- reshapeArray xs xPixel = chunksOf xPixel xs


--figure1 = getEscapeStep (getFractalSequence mandelbrotSet (getPixelPoint 128 64 (-3.5,-2.1) (1.5,2.1))) 100
--figure2 = reshapeArray figure1 128
