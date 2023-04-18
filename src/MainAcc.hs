{-# LANGUAGE FlexibleContexts
           , TypeFamilies #-}

module Main where

import ModelAcc
import ControllerAcc
import ViewAcc
import ConsoleAcc

import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Interface.IO.Interact hiding (Vector)
import Graphics.Gloss.Interface.IO.Game hiding (Vector)
import Data.Array.Accelerate

import Prelude as P


main :: IO ()
main = do
    gd <- getGenData
    let world = startWorld gd

    playIO (InWindow "Fractals" (screenHeight, screenWidth) (0, 0))
            black           -- Background color
            60              -- Frames per second
            world           -- Initial state
            drawHandler     -- View function
            inputHandler    -- Event function
            stepHandlerAcc  -- Step function


startWorld :: GeneratorData -> World
startWorld gd = MkWorld
    worldMatrix
    gd
    (1, (0, 0))
    Blank
    True
  where
    fracFunc = makeFractalFunctionAcc False 2
    worldMatrix = fromList (Z:.x:.y) flatList :: Matrix (Float, Float)
      where
        x = P.length gridPoint
        y = P.length (head gridPoint)
        flatList = concat gridPoint
        gridPoint = [[(P.fromIntegral $ x - halfScrW, P.fromIntegral $ y - halfScrH) 
                     | x <- [0..screenWidth -1]] 
                      | y <- [0..screenHeight-1]]
