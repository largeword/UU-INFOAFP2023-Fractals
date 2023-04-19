{-# LANGUAGE FlexibleContexts
           , TypeFamilies #-}

module MainAcc (main) where

import Prelude as P
import ModelAcc
import ControllerAcc
import ViewAcc
import ConsoleAcc

import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Interface.IO.Interact hiding (Vector)
import Graphics.Gloss.Interface.IO.Game hiding (Vector)
import Data.Array.Accelerate


-- | Main entry point for the program
--   Start by console operations to obtain user input, then run the 'game'
--   We use playIO instead of play to be able to have our inputHandler be in IO
--   This allows us to re-run getGenData at the press of a button,
--   while the app is running
main :: IO ()
main = do
    gd <- getGenData
    let world = startWorld gd

    playIO (InWindow "Fraskell" (screenHeight, screenWidth) (0, 0))
            black           -- Background color
            60              -- Frames per second
            world           -- Initial state
            drawHandler     -- View function
            inputHandler    -- Event function
            stepHandlerAcc  -- Step function


-- | Given the generatorData obtained from the user, this creates a starter World
--   see Model for more details on what this all includes
--   The worldMatrix here is generated as an accelerate Matrix structure right away
--   This avoids having to go back and forth several times during our step.
startWorld :: GeneratorData -> World
startWorld gd = MkWorld
    worldMatrix
    gd
    (1, (0, 0))
    Blank
    True
  where
    worldMatrix = fromList (Z:.x:.y) flatList :: Matrix (Float, Float)
      where
        x = screenWidth
        y = screenHeight
        flatList  = concat worldGrid
        worldGrid = [[(P.fromIntegral $ x - halfScrW, P.fromIntegral $ y - halfScrH)
                     | x <- [0..screenWidth -1]]
                      | y <- [0..screenHeight-1]]
