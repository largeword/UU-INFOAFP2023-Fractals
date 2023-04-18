{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleContexts, TypeFamilies, TypeOperators, FlexibleContexts #-}

module ControllerAcc where

import ModelAcc
import ViewAcc
import Generator
import Console

import Data.List
import Data.Ord hiding (Down)

import Graphics.Gloss hiding (scale)
import Graphics.Gloss.Interface.IO.Interact hiding (scale)

import Data.Array.Accelerate              as A
import Data.Array.Accelerate.LLVM.Native  as CPU
import Data.Array.Accelerate.LLVM.PTX     as GPU
import Data.Array.Accelerate.Interpreter  as I

import GeneratorAcc as GA

-- import Debug.Trace


-- | InputHandler is responsible for all inputs that happen
--   It contains three cases, each explained in further detail below
inputHandler :: Event -> World -> IO World

-- | This is a special (and WIP) case.  It involves an action that happens
--   immediately once.
--   Such as selecting a point for which to generate the Julia
--   or (potentially in the future) using the mousewheel to zoom
inputHandler (EventKey (MouseButton LeftButton) Down _ (x, y)) w = 
  let newData = (gData w) { offset = (x / 125, y / 125)}
   in return $ w {gData = newData, isChanged = True}
inputHandler (EventKey (Char        'r'       ) Down _ _     ) w = 
  return $ w { transform = (1, (0, 0))
             , isChanged = True}
inputHandler (EventKey (Char        ' '       ) Down _ _     ) w =
  do gd <- getGenData
     return $ w { gData      = gd
                , isChanged = True}


-- | This is a case for events that happen every tick, as long as a button is held
--   Such as moving using the buttons
--   The first function of this case matches on a button pressed *Down*,
--   in which case it is added to the event list
inputHandler e@(EventKey _ Down _ _) w =
  case parseEvent e of 
    Nothing -> return w
    Just ev -> let tf = doEvent ev (transform w)
                in return $ w { transform = tf
                              , isChanged = True }

-- | Lastly, wildcard pattern returns the input world
inputHandler _ w = return w


-- | Straight-forward function to parse the Gloss event type into a custom data
--   defining an actionable type
--   We match on Event rather than Key, to allow potential future expansions
--   If a key is unknown, return Nothing
parseEvent :: Event -> Maybe EventAction
parseEvent (EventKey key _ _ _) = case key of
                           Char 'w' -> Just $ Move Up'
                           Char 'a' -> Just $ Move Left'
                           Char 's' -> Just $ Move Down'
                           Char 'd' -> Just $ Move Right'
                           Char 'q' -> Just $ Zoom Out
                           Char 'e' -> Just $ Zoom In

                           SpecialKey KeyUp    -> Just $ Move Up'
                           SpecialKey KeyLeft  -> Just $ Move Left'
                           SpecialKey KeyDown  -> Just $ Move Down'
                           SpecialKey KeyRight -> Just $ Move Right'

                           _ -> Nothing
parseEvent                 _ =  Nothing


-- | Accelerated version
stepHandlerAcc :: Float -> World -> IO World
stepHandlerAcc _ w@(MkWorld screen d tf _ True) =
  let picture  = draw                          -- turned into a pretty picture 'v'
               . GA.arr2Grid $ CPU.run         -- running accelerated process  :: Grid (Point, Color)
               . A.zipWith (,) (A.use screen)  -- zipping colour with position :: Matrix (Point, Color)
               . getColorsAcc colorList        -- turned into colored grid     :: Matrix Color
               . GA.getEscapeStepsAcc          -- turned into numbered grid    :: Matrix Int
               . GA.getSequencesAcc            -- turned into sequenced grid   :: Matrix [Point]
               . (`scaleAcc` tf)               -- Scaled to our parameters     :: Matrix Point
               $ A.use screen                  -- The unscaled default screen  :: Matrix Point
   in return $ w { currentPicture = picture
               , isChanged      = False }

-- | Default case - nothing is changed
stepHandlerAcc _ w = w


eventStep :: [EventAction] -> (ZoomScale, Translation) -> (ZoomScale, Translation)
eventStep es transf = foldr doEvent transf es

doEvent :: EventAction -> (ZoomScale, Translation) -> (ZoomScale, Translation)
doEvent e (z, (tx, ty)) = case e of
  Move Up'   -> (z     , (tx     , ty + fy))
  Move Left' -> (z     , (tx - fx, ty     ))
  Move Down' -> (z     , (tx     , ty - fy))
  Move Right'-> (z     , (tx + fx, ty     ))
  Zoom In    -> (z - zf, (tx     , ty     ))
  Zoom Out   -> (z + zf, (tx     , ty     ))
  where    
    zf = 0.1
    fx = 0.001 / (z * scaleFactor)
    fy = 0.001 / (z * scaleFactor)
    



-- | Perform linear transformation with given zooming scale, r offset, i offset.
scale :: Grid Point -> (ZoomScale, Translation) -> Grid Point
scale grid (zoom, (rOff, iOff)) = gridMap f grid
  where
    f :: Point -> Point
    f (r, i) = (r * zoom' + rOff, i * zoom' + iOff)
    zoom' = zoom * scaleFactor


scaleAcc :: Acc (Matrix (Float, Float)) -> (ZoomScale, Translation) -> Acc (Matrix (Float, Float))
scaleAcc gridAcc (zoom, (rOff, iOff)) = A.map f gridAcc
  where
    zoom' = (A.lift zoom) A.* (A.lift scaleFactor)
    f :: Exp (Float, Float) -> Exp (Float, Float)
    f point = let r = A.fst point 
                  i = A.snd point in
              A.lift (r A.* zoom' A.+ (A.lift rOff), i A.* zoom' A.+ (A.lift iOff))

