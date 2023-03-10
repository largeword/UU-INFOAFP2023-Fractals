module GenerateFractals where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.List
import Data.List.Split
import Data.Function


-- complex number iteration
getNextZ :: Point -> Point -> Point
getNextZ (cx, cy) (zx, zy) = (zx ** 2 - zy ** 2 + cx, 2 * zx * zy + cy)


iteration :: (a -> a) -> a -> [a]
iteration = fix $ \iteration' f x -> (f x) : iteration' f (f x)


-- generate infinite sequence of given point
mandelbrotSet :: Point -> [Point]
mandelbrotSet c = iteration (getNextZ c) (0, 0)

juliaSet :: Point -> Point -> [Point]
juliaSet c z = iteration (getNextZ c) z


-- check whether fractal sequence escape
crossThreshold :: Point -> Bool
crossThreshold (zx, zy) = (zx ** 2 + zy ** 2) < 9


-- generate possition for each pixel
getPixelPoint :: Int -> Int -> Point -> Point -> [Point]
getPixelPoint xPixels yPixels (xMin, yMin) (xMax, yMax) = map (,) [xMin, xMin + xGap .. xMax] <*> [yMin, yMin + yGap .. yMax]
                                                          where xGap = (xMax - xMin) / fromIntegral (xPixels - 1)
                                                                yGap = (yMax - yMin) / fromIntegral (yPixels - 1)


-- generate fractal sequence for each pixel
getFractalSequence :: (Point -> [Point]) -> [Point] -> [[Point]]
getFractalSequence fractalIteration pixelPoint = map fractalIteration pixelPoint


-- find escape step for each pixel
getEscapeStep :: [[Point]] -> Int -> [Int]
getEscapeStep pixelSequence nonEscapeNum = map (length . filter (== True) . take nonEscapeNum) (map (map crossThreshold) pixelSequence)


-- reshape list/array
reshapeArray :: [a] -> Int -> [[a]]
reshapeArray xs xPixel = chunksOf xPixel xs


figure1 = getEscapeStep (getFractalSequence mandelbrotSet (getPixelPoint 128 64 (-3.5,-2.1) (1.5,2.1))) 100
figure2 = reshapeArray figure1 128
