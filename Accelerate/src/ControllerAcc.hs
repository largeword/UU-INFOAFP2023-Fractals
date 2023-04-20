{-# LANGUAGE FlexibleContexts
           , TypeFamilies #-}

module ControllerAcc (inputHandler, stepHandlerAcc, scaleAcc) where

import ModelAcc
import GeneratorAcc
import ViewAcc
import ConsoleAcc

import Data.List
import Data.Ord hiding (Down)

import Graphics.Gloss hiding (scale)
import Graphics.Gloss.Interface.IO.Interact hiding (scale)
import Data.Array.Accelerate              as A
import Data.Array.Accelerate.LLVM.Native  as CPU
import Data.Array.Accelerate.LLVM.PTX     as GPU

import Debug.Trace


-- | InputHandler is responsible for all inputs that happen
--   It contains three cases, each explained in further detail below
inputHandler :: Event -> World -> IO World


-- | This is a case involving an action that happens immediately once.
--   Such as selecting a point for which to generate the Julia:
--   'lmb' selects the current mouse position as new offset point
inputHandler (EventKey (MouseButton LeftButton) Down _ (x, y)) w =
  let newData = (gData w) { offset = lift (x * scaleFactor, y * scaleFactor)}
   in return $ w { gData     = newData
                 , isChanged = True}

-- | 'r' key resets transformation and zoom scaling
inputHandler (EventKey (Char        'r'       ) Down _ _     ) w =
  return $ w { transform = (1, (0, 0))
             , isChanged = True}

-- | 'g' key re-prompts the fractal generatorData setup
inputHandler (EventKey (Char        'g'       ) Down _ _     ) w =
  do gd <- getGenData
     let _ = gd `seq` undefined
     return $ w { gData     = gd
                , isChanged = True}


-- | This is a case for events that should happen every tick, as long as a button is held
--   Such as moving using the buttons
--   However, due to the tool's performance, this feature was eventually scrapped:
--   Without live updates, it is infeasable to expect the user to know when to release
--   Practically now, this case is no different from the first
--   However, the infrastructure to change this eventually does remain
--   in which case this function is to add the event to a list
--   and a secondary function matching on Up is to remove the event from this list
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
--   Like the function above, however, much of this function is vestigial
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

-- | One more vestigial function, used to handle a single event
--   updating zoom and translation respectively depending on the nature of the event
--   There used to be a function `eventStep :: [EventAction] -> (ZoomScale, Translation) -> (ZoomScale, Translation)`
--   which would fold over this function to apply all events we memorised
--   Since we no longer memorise anything, that function was removed
--   And this function is just called on its own
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



-- | Accelerated version of the stepHandler
--   The function handles a single step in our world
--   We compute and render the fractal in here,
--   but only if our boolean flag is set to True
--   When reading the step-by step at the side, please be mindful that this happens
--   in inverse order: From bottom to top
stepHandlerAcc :: Float -> World -> IO World
stepHandlerAcc _ w@(MkWorld screen d tf _ True) = do
  putStrLn "Rendering... Please stand by"
  let picture = draw                            -- turned into a pretty picture 'v'
              . arr2Grid $ CPU.run              -- running accelerated process   :: Grid (Point, Color)
              . A.zip (A.use screen)            -- zipping colour with position  :: Matrix (Point, Color)
              . getColorsAcc (A.use colorList)  -- turned into colored matrix    :: Matrix Color
              . getEscapeStepsAcc               -- turned into numbered matrix   :: Matrix Int
              . getSequencesAcc d               -- turned into sequenced matrix  :: Cubic Point // Matrix [Point]
              . (`scaleAcc` tf)                 -- Scaled to our parameters      :: Matrix Point
              $ A.use screen                    -- The unscaled default screen   :: Matrix Point
  return $ w { currentPicture = picture
             , isChanged      = False }

-- | Default case - nothing is changed
stepHandlerAcc _ w = return w


-- | Accelerated version of `scale`
--   Perform linear transformation with given zooming scale, r offset, i offset.
scaleAcc :: Acc (Matrix (Float, Float)) -> (ZoomScale, Translation) -> Acc (Matrix (Float, Float))
scaleAcc gridAcc (zoom, (rOff, iOff)) = A.map f gridAcc
  where
    zoom' = A.lift $ zoom * scaleFactor
    f :: Exp (Float, Float) -> Exp (Float, Float)
    f point = let r = A.fst point
                  i = A.snd point
               in A.lift (r A.* zoom' A.+ A.lift rOff, i A.* zoom' A.+ A.lift iOff)
