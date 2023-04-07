module Main where

import Model
import ControllerAcc
import View
import Console

import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Interface.IO.Interact hiding (Vector)
import Debug.Trace

main :: IO ()
main = play (InWindow "Fractals" (screenHeight, screenWidth) (0, 0))        -- Or Fullscreen
            black                                                           -- Background color
            60                                                              -- Frames per second
            startWorld                                                      -- Initial state
            drawHandler                                                     -- View function
            inputHandler                                                    -- Event function
            stepHandlerAcc                                                     -- Step function


startWorld :: World
startWorld = MkWorld
    [[(fromIntegral $ x - halfScrW, fromIntegral $ y - halfScrH) | x <- [0..screenWidth-1]] | y <- [0..screenHeight-1]]
    (GenData { position = (0,0), escapeRadius = 100, parameter = VarZ, offset = (0,0), func = fracFunc})
    (1, (0, 0))
    Blank
    True
    where
        fracFunc = makeFractalFunction False 2 
        -- if 1st arg is True, then absolute value will be taken of z
        -- second arg represents polynomial degree
