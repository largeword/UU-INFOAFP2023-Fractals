module Main where

import Model
import Controller
import View
import Console

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Debug.Trace

main :: IO ()
main = do
    gd <- getGenData
    let world = startWorld gd

    play (InWindow "Fractals" (screenHeight, screenWidth) (0, 0))        -- Or Fullscreen
          black                                                          -- Background color
          60                                                             -- Frames per second
          world                                                          -- Initial state
          drawHandler                                                    -- View function
          inputHandler                                                   -- Event function
          stepHandler                                                    -- Step function


startWorld :: GeneratorData -> World
startWorld gen = MkWorld
    [[(fromIntegral $ x - halfScrW, fromIntegral $ y - halfScrH) | x <- [0..screenWidth-1]] | y <- [0..screenHeight-1]]
    gen
    (1, (0, 0))
    Blank
    True
