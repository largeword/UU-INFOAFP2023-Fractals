module View where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact


drawHandler :: World -> Picture
drawHandler (MkWorld _ _ _ _ p _) = p


-- | Function called in the last step of the step
--   Input parameters are (unscaled) grid of points and grid of colours
--   By zipping them together we can easily map pointToPicture over them
--   Then the result is condensed into a single picture
draw :: [[Point]] -> [[Color]] -> Picture
draw screen cols = let pointAndColour = zipWith zip screen cols
                       pics           = map (map pointToPicture) pointAndColour
                    in Pictures . map Pictures $ pics

-- | Very simple: generate a pixel point, and give it the right colour and translation
--   Because the translation is wrt the screen coordinates as opposed to view coordinates,
--   these need to be the unscaled versions.
pointToPicture :: (Point, Color) -> Picture
pointToPicture ((x, y), c) = Translate x y $ Color c $ Circle 1