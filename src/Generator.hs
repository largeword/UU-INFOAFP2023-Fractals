module Generator (getSequences, getEscapeSteps) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.List
import Data.List.Split
import Data.Function

import Model

-- | Computes a fractal pattern based on fractal meta data and a given point
generateFractal :: GeneratorData -> Point -> [Point] 
generateFractal genData pt = case parameter genData of
  VarZ -> computeFractal genData {position = pt} 
  VarC -> computeFractal genData {offset   = pt}
  _    -> error "Other functionalities are not yet defined."    

-- | Generates the infinite fractal set, defining the iterations
--   according to the generation data provided
computeFractal :: GeneratorData -> [Point]
computeFractal genData = let fracFunc = func     genData 
                             c        = offset   genData
                             z        = position genData
                         in 
                           iterate (`fracFunc` c) z
                            

-- | Given a scaled grid, compute the sequences of iterations
getSequences :: GeneratorData -> Grid Point -> Grid [Point]
getSequences genData grid = gridMap (generateFractal genData) grid


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
