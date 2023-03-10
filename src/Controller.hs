module Controller where

import Model
import View

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

inputHandler :: Event -> World -> World
inputHandler _ w = w

stepHandler :: Float -> World -> World
stepHandler _ w@(MkWorld screen _ _ _ _ True) = w { currentPicture = draw screen undefined, isChanged = False }
stepHandler _ w = w





-- KEY MAPPINGS

-- Panning: translation magnitudes (eventually within window app?)

    -- in x direction

    -- in y direction

-- Zoomscaling

-- Later on: loading config document or querying user for input values via console (implement in main.hs in do-block)