module Controller where

import Model
import View
import Generator
import Graphics.Gloss hiding (scale)
import Graphics.Gloss.Interface.IO.Interact hiding (scale)

import Debug.Trace
import Data.List
import Data.Ord

inputHandler :: Event -> World -> World
inputHandler _ w = w


-- | Handles a single step in our world
--   We compute and render the fractal in here,
--   only if our boolean flag is set to True
stepHandler :: Float -> World -> World
stepHandler _ w@(MkWorld screen _ z t _ True) =
    let picture = draw screen          -- turned into a pretty picture 'v'
                . getColors colorList  -- turned into colored grid    :: Grid Color
                . getEscapeSteps 100   -- turned into numbered grid   :: Grid Int
                . getSequences         -- turned into sequenced grid  :: Grid [Point]
                . (`scale` (z, t))     -- Scaled to our parameters    :: Grid Point
                $ screen               -- The unscaled default screen :: Grid Point
     in w { currentPicture = picture, isChanged = False }

-- | Default case - nothing is changed
stepHandler _ w = w


-- | Perform linear transformation with given zooming scale, r offset, i offset.
scale :: Grid Point -> (Float, (Float, Float)) -> Grid Point
scale grid (zoom, (rOff, iOff)) = gridMap f grid
  where
    f :: Point -> Point
    f (r, i) = (r * zoom + rOff, i * zoom + iOff)

{- The following expression is used to get Min and Max for Grid Point
xOldMin = minimumBy (comparing (!!0)) grid
xOldMax = maximumBy (comparing (!!0)) grid
yOldMin = minimumBy (comparing (!!1)) grid
yOldMax = maximumBy (comparing (!!1)) grid
-}




-- KEY MAPPINGS

-- Panning: translation magnitudes (eventually within window app?)

    -- in x direction

    -- in y direction

-- Zoomscaling

-- Later on: loading config document or querying user for input values via console (implement in main.hs in do-block)