module View where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import GHC.Float


drawHandler :: World -> Picture
drawHandler (MkWorld _ _ _ _ p _) = p


-- | Function called in the last step of the step
--   Input parameters are (unscaled) grid of points and grid of colours
--   By zipping them together we can easily map pointToPicture over them
--   Then the result is condensed into a single picture
draw :: Grid Point -> Grid Color -> Picture
draw screen cols = let pointAndColour = zipWith zip screen cols
                       pics           = map (map pointToPicture) pointAndColour
                    in Pictures . map Pictures $ pics

-- | Very simple: generate a pixel point, and give it the right colour and translation
--   Because the translation is wrt the screen coordinates as opposed to view coordinates,
--   these need to be the unscaled versions.
pointToPicture :: (Point, Color) -> Picture
pointToPicture ((x, y), c) = Translate x y $ Color c $ Circle 1


-- | Default color list
--   https://colorswall.com/palette/128774
colorList :: [Color]
colorList = [makeColorI (rgb' !! 0) (rgb' !! 1) (rgb' !! 2) 1 | rgb' <- rgb ]
            where rgb = [[43, 192, 232], [246, 203, 102], [72, 68, 152], 
                         [99, 167, 94], [160, 172, 180], [68, 62, 94]]


-- | This function maps the Grid of escaping steps into the corresponding colors
getColors :: [Color] -> Grid Int -> Grid Color
getColors colors grid = gridMap (float2Color colors) grid'
                        where grid' :: Grid Float
                              grid' = rescaleGrid2ColorRange colors grid


-- | This function maps the input Grid Int into the right range of color list with decimals
--   https://intellipaat.com/community/33375/convert-a-number-range-to-another-range-maintaining-the-ratio
rescaleGrid2ColorRange :: [Color] -> Grid Int -> Grid Float
rescaleGrid2ColorRange colors grid = gridMap (\x -> ((int2Float x) - minGrid) 
                                              * (lenColor - 1) / (maxGrid - minGrid)) grid
                                     where maxGrid = int2Float (maximum (maximum grid)); 
                                           minGrid = int2Float (minimum (minimum grid));
                                           lenColor = int2Float (length colors);


-- | This function takes a color list and a float number, then find the nearest two colors in the list 
--   according to float as index, and mix these colors with the right proportion
float2Color :: [Color] -> Float -> Color
float2Color colors x = mixColors mixProtion
                                     (1 - mixProtion)
                                     (colorList !! (floor x)) 
                                     (colorList !! (ceiling x))
                       where mixProtion = x - (int2Float (floor x))
