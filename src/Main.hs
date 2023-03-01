module Main where


import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

main :: IO ()
main = play (InWindow "Fractals" (500, 500) (0, 0))        -- Or Fullscreen
            black                                          -- Background color
            60                                             -- Frames per second
            World                                          -- Initial state
            drawHandler                                    -- View function
            inputHandler                                   -- Event function
            stepHandler                                    -- Step function


data World = World

drawHandler :: World -> Picture
drawHandler _ = Color red $ ThickCircle 15 15

inputHandler :: Event -> World -> World
inputHandler _ w = w

stepHandler :: Float -> World -> World
stepHandler _ w = w