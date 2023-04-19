module View (drawHandler, draw, getColors) where

import Model

import GHC.Float (int2Float)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact


-- | Main tick draw function
--   Returns the cached picture p
drawHandler :: World -> IO Picture
drawHandler (MkWorld _ _ _ p _) = return p


-- | Function called in the last step of the step
--   Input parameters are (unscaled) grid of points and grid of colours
--   By zipping them together we can easily map pointToPicture over them
--   Then the result is condensed into a single picture
draw :: Grid Point -> Grid Color -> Picture
draw screen cols = let pointAndColour = zipWith zip screen cols
                       pics           = gridMap pointToPicture pointAndColour
                    in Pictures . map Pictures $ pics

-- | Very simple: generate a pixel point, and give it the right colour and translation
--   Because the translation is wrt the screen coordinates as opposed to view coordinates,
--   these need to be the unscaled versions.
pointToPicture :: (Point, Color) -> Picture
pointToPicture ((x, y), c) = Translate x y $ Color c $ Circle 1


-- | This function maps the Grid of escaping steps into the corresponding colors
getColors :: [Color] -> Grid Int -> Grid Color
getColors colors grid = let grid' = rescaleGrid2ColorRange colors grid
                         in gridMap (float2Color colors) grid'


-- | This function maps the input Grid Int into the right range of color list with decimals
rescaleGrid2ColorRange :: [Color] -> Grid Int -> Grid Float
rescaleGrid2ColorRange colors grid =
  let gridMax  = int2Float . maximum    . concat $ grid
      gridMin  = int2Float . minimum    . concat $ grid
      colMax   = int2Float . subtract 1 . length $ colors
      f        = \x -> if gridMax == 0 
                       then 0 
                       else (int2Float x - gridMin)  * colMax / (gridMax - gridMin)  -- 0/0 = NaN
   in gridMap f grid


-- | This function takes a color list and a float number, then find the nearest two colors in the list 
--   according to float as index, and mix these colors with the right proportion
float2Color :: [Color] -> Float -> Color
float2Color colors x = let x' = if isNaN x 
                                then int2Float (length colors - 1) 
                                else x
                           floorX   = floor x'
                           ceilingX = ceiling x'
                           mixProportion = (x' -) . int2Float $ floorX
                        in mixColors mixProportion
                                     (1 - mixProportion)
                                     (colorList !! floorX) 
                                     (colorList !! ceilingX)
