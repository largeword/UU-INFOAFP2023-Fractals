module Generator (getSequences, getEscapeSteps) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.List
import Data.List.Split
import Data.Function

import Model

-- | A type synonym to define the abstract point calculation function
--   The first argument is the kind of fractal to be generated, the second 
--   is the degree n of the polynomial z^n in the body of the
--   function. For our purposes, n is kept constant. The third is the complex paramater c
--   and the fourth is the starting point z.

type ZFunction = Fractal -> Int -> Point -> Point -> Point    
-- What do we want as a starting ZFunction?

-- | Implementation of the fractal formula per fractal type
nextFractalPoint :: ZFunction

-- Varies z in the function z' = z^2 + c
nextFractalPoint MBrot _ (cr, ci) (zr, zi)
  = let (z2r, z2i) = ( zr ** 2 - zi ** 2          -- real
                     , 2 * zr * zi       )        -- imag
    in ( z2r + cr                                 -- real
       , z2i + ci )                               -- imag

---- Alternative definition for Julia set?
-- Varies z in the function z' = z^2 + c
-- nextFractalPoint Julia (zr, zi) (cr, ci) _   -- single-Julia set implementation
--   = let (z2r, z2i) = ( zr ** 2 - zi ** 2          -- real
--                      , 2 * zr * zi       )        -- imag
--     in ( z2r + cr                                 -- real
--        , z2i + ci )                               -- imag

---- Questions to be answered 
-- maybe use the pseudocode on wikipedia page to approximate polynomials of higher degrees 
-- than 2 for multi-Julia sets?
-- can't we use the same function for MBrot and single-Julia? It's the same calculation,
-- only different inputs change (different starting points for each iteration of MBrot
-- and different points p for each Julia set?)


-- | Generates a list of length n, defining the iterations
--   according to the generation data provided
getIterations :: GeneratorData -> [Point]
getIterations genData = let fType = fractalType genData
                            n     = degree      genData
                            c     = cplexParam  genData
                            z     = position    genData
                        in 
                          iterate (nextFractalPoint fType n c) z
                           

------ Temporary ----------------

-- generate iterations of a fixed-c
fixedC :: GeneratorData -> Point -> [Point] 
fixedC genData z = getIterations genData {position = z} 
                  
      

-- generate iterations of a fixed-z
fixedZ :: GeneratorData -> Point -> [Point] -- maybe give a slider to user/input field for changing c
fixedZ genData c = getIterations genData{cplexParam = c} 
                


----------------------------------


-- | Given a scaled grid, compute the sequences of iterations
getSequences :: GeneratorData -> Grid Point -> Grid [Point]
getSequences genData grid = gridMap (fixedZ genData) grid


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
    crossThreshold :: Point -> Bool
    crossThreshold (zx, zy) = (zx ** 2 + zy ** 2) < 100







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
