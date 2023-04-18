{-# LANGUAGE DeriveAnyClass, 
             DeriveGeneric, 
             FlexibleContexts, 
             TypeFamilies, 
             TypeOperators, 
             FlexibleContexts, 
             StandaloneDeriving, 
             UndecidableInstances, 
             AllowAmbiguousTypes #-}

module ViewAcc (drawHandler, draw, getColorsAcc) where

import ModelAcc

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import GHC.Float (int2Float)

import Data.Array.Accelerate              as A


drawHandler :: World -> IO Picture
drawHandler (MkWorld _ _ _ p _) = return p


-- | Function called in the last step of the step
--   Input parameters are (unscaled) grid of points and grid of colours
--   By zipping them together we can easily map pointToPicture over them
--   Then the result is condensed into a single picture
draw :: Grid (Point, ColorAcc) -> Picture
draw pointAndCol = let pics = gridMap pointToPicture pointAndCol
                    in Pictures . Prelude.map Pictures $ pics

-- | Very simple: generate a pixel point, and give it the right colour and translation
--   Because the translation is wrt the screen coordinates as opposed to view coordinates,
--   these need to be the unscaled versions.
pointToPicture :: (Point, ColorAcc) -> Picture
pointToPicture ((x, y), c) = let (r, g, b, a) = c
                                 c'           = makeColor r g b a
                              in Translate x y $ Color c' $ Circle 1


               


-- | This function is used to color every pixel based on their escaping step
getColorsAcc :: Acc (A.Vector ColorAcc) 
                -> Acc (Matrix Int) 
                -> Acc (Matrix ColorAcc)
getColorsAcc colorsAcc gridAcc = let grid' = rescaleGrid2ColorRangeAcc colorsAcc gridAcc
                                 in  A.map (float2ColorAcc colorsAcc) grid'


-- | This function is used to rescale the escaping step into color index range
rescaleGrid2ColorRangeAcc :: Acc (A.Vector ColorAcc) 
                             -> Acc (Matrix Int) 
                             -> Acc (Matrix Float)
rescaleGrid2ColorRangeAcc colorsAcc gridAcc = 
  let gridAccFlat = A.flatten gridAcc
      gridAccMax  = A.toFloating ((A.maximum gridAccFlat) A.!! 0) :: Exp Float
      gridAccMin  = A.toFloating ((A.minimum gridAccFlat) A.!! 0) :: Exp Float
      colMax      = A.toFloating . A.subtract 1 . A.length $ colorsAcc :: Exp Float 
      f :: Exp Int -> Exp Float
      f           = \x -> A.ifThenElse (gridAccMax A.== 0) 
                                       (0.0) 
                                       (((A.toFloating x) A.- gridAccMin)  A.* (colMax) A./ (gridAccMax - gridAccMin))
   in A.map f gridAcc


-- | This function takes a color list and a float number, then find the nearest two colors in the list 
--   according to float as index, and mix these colors with the right proportion
float2ColorAcc :: Acc (A.Vector ColorAcc) -> Exp Float -> Exp ColorAcc
float2ColorAcc colorsAcc x = let x' = A.ifThenElse (A.isNaN x) 
                                                   (A.toFloating ((A.length colorsAcc) A.- 1)) 
                                                   (x) :: Exp Float
                                 floorX = A.floor x'
                                 ceilingX = A.ceiling x'
                                 mixProportion = (x' A.-) . A.toFloating $ floorX :: Exp Float
                             in  mixColorsAcc mixProportion
                                              (1 - mixProportion)
                                              (colorsAcc A.!! floorX) 
                                              (colorsAcc A.!! ceilingX)


-- | Source: https://hackage.haskell.org/package/gloss-1.13.2.2/docs/src/Graphics.Gloss.Data.Color.html
--   Based on the original mixColor, make it fit with Acc Array
--   Arguments: Proportion of first color, Proportion of second color, First color, Second color
--   Returns: Resulting color
mixColorsAcc :: Exp Float -> Exp Float -> Exp ColorAcc -> Exp ColorAcc -> Exp ColorAcc
mixColorsAcc m1 m2 c1 c2 = let (T4 r1 g1 b1 a1) = c1
                               (T4 r2 g2 b2 a2) = c2

                               -- Normalise mixing proportions to ratios.
                               m12 = m1 A.+ m2
                               m1' = m1 A./ m12
                               m2' = m2 A./ m12

                               -- Colors components should be added via sum of squares,
                               -- otherwise the result will be too dark.
                               r1s = r1 A.* r1;    r2s = r2 A.* r2
                               g1s = g1 A.* g1;    g2s = g2 A.* g2
                               b1s = b1 A.* b1;    b2s = b2 A.* b2

                           in  A.lift ((A.sqrt (m1' A.* r1s A.+ m2' A.* r2s)), 
                                       (A.sqrt (m1' A.* g1s A.+ m2' A.* g2s)), 
                                       (A.sqrt (m1' A.* b1s A.+ m2' A.* b2s)), 
                                       ((m1 A.* a1   A.+ m2 A.* a2) A./ m12))
