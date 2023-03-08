module Main where

import Model
import Controller
import View

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

main :: IO ()
main = play (InWindow "Fractals" (500, 500) (0, 0))        -- Or Fullscreen
            black                                          -- Background color
            60                                             -- Frames per second
            MkWorld                                          -- Initial state
            drawHandlerr                                    -- View function
            inputHandler                                   -- Event function
            stepHandler                                    -- Step function


