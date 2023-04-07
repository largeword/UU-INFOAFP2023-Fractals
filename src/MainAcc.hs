module Main where

import Model
import ControllerAcc
import View
import Console

import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Interface.IO.Interact hiding (Vector)
import Data.Array.Accelerate
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
    worldMatrix
    (GenData { position = (0,0), escapeRadius = 100, parameter = VarZ, offset = (0,0), func = fracFunc})
    (1, (0, 0))
    Blank
    True
    where
        fracFunc = makeFractalFunction False 2 
        worldMatrix = fromList (Z:.x:.y) flatList :: Matrix (Float, Float)
          where  x = Prelude.length gridPoint
                 y = Prelude.length (head gridPoint)
                 flatList = concat gridPoint
                 gridPoint = [[(fromIntegral $ x - halfScrW, fromIntegral $ y - halfScrH) 
                              | x <- [0..screenWidth -1]] 
                              | y <- [0..screenHeight-1]]
