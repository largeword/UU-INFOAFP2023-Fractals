module Controller where

import Model
import View
import Generator
import Graphics.Gloss hiding (scale)
import Graphics.Gloss.Interface.IO.Interact hiding (scale)

inputHandler :: Event -> World -> World
inputHandler _ w = w


-- | Handles a single step in our world
--   We compute and render the fractal in here,
--   only if our boolean flag is set to True
stepHandler :: Float -> World -> World
stepHandler _ w@(MkWorld screen _ z t _ True) =
    let picture = draw screen        -- turned into a pretty picture 'v'
                . getColours cols    -- turned into coloured grid   :: Grid Color
                . getEscapeSteps 25  -- turned into numbered grid   :: Grid Int
                . getSequences       -- turned into sequenced grid  :: Grid [Point]
                . (`scale` (z, t))   -- Scaled to our parameters    :: Grid Point
                $ screen             -- The unscaled default screen :: Grid Point
     in w { currentPicture = picture, isChanged = False }

-- | Default case - nothing is changed
stepHandler _ w = w


scale :: Grid Point -> (Float, (Int, Int)) -> Grid Point
scale = undefined





-- KEY MAPPINGS

-- Panning: translation magnitudes (eventually within window app?)

    -- in x direction

    -- in y direction

-- Zoomscaling

-- Later on: loading config document or querying user for input values via console (implement in main.hs in do-block)