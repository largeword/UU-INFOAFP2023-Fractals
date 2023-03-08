module View where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact


drawHandler :: World -> Picture
drawHandler (MkWorld _ _ _ _ p _) = p

draw :: [[Point]] -> Picture
draw screen = pictures . map pictures $ map (map pointToColor) screen

pointToColor :: Point -> Picture
pointToColor (x, y) | isEven (x + y) = Translate x y $ Color red   $ Circle 1
                    | otherwise      = Translate x y $ Color green $ Circle 1
  where
    isEven = even . round