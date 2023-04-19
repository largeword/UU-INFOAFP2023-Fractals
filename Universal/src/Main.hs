module Main (main) where

import Model
import Controller
import View
import Console

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.IO.Game


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
          black         -- Background color
          60            -- Frames per second
          world         -- Initial state
          drawHandler   -- View function
          inputHandler  -- Event function
          stepHandler   -- Step function


-- | Given the generatorData obtained from the user, this creates a starter World
--   see Model for more details on what this all includes
--   It's made as a 2D list of lists, a Grid datatype
--   The size of the grid corresponds to our screen dimensions,
--   shifted by half that to keep (0, 0) in the middle.
startWorld :: GeneratorData -> World
startWorld gd = MkWorld
    worldGrid
    gd
    (1, (0, 0))
    Blank
    True
  where
    worldGrid = [[(fromIntegral $ x - halfScrW, fromIntegral $ y - halfScrH) 
                 | x <- [0..screenWidth -1]] 
                  | y <- [0..screenHeight-1]]
