module Main where

import Model
import Controller
import View

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

main :: IO ()
main = play (InWindow "Fractals" (screenHeight, screenWidth) (0, 0))        -- Or Fullscreen
            black                                                           -- Background color
            60                                                              -- Frames per second
            startWorld                                                      -- Initial state
            drawHandler                                                     -- View function
            inputHandler                                                    -- Event function
            stepHandler                                                     -- Step function


