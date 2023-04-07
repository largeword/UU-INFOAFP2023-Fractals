{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleContexts, TypeFamilies, TypeOperators, FlexibleContexts #-}

module Main where

import ModelAcc
import ControllerAcc
import ViewAcc
import Console

import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Interface.IO.Interact hiding (Vector)

--import Debug.Trace

import Data.Array.Accelerate

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
                 gridPoint = [[(Prelude.fromIntegral $ x - halfScrW, Prelude.fromIntegral $ y - halfScrH) 
                              | x <- [0..screenWidth -1]] 
                              | y <- [0..screenHeight-1]]
