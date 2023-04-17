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
main = do
    gd <- getGenData
    let world = startWorld gd

    playIO (InWindow "Fractals" (screenHeight, screenWidth) (0, 0))
            black                                                        -- Background color
            60                                                           -- Frames per second
            world                                                        -- Initial state
            drawHandle                                                   -- View function
            inputHandler                                                 -- Event function
            stepHandlerAcc                                                -- Step function


startWorld :: GeneratorData -> World
startWorld gd = MkWorld
    worldMatrix
    gd
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
