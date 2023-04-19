module Controller (inputHandler, stepHandler) where

import Model
import Generator
import View
import Console

import Data.List
import Data.Ord hiding (Down)

import Graphics.Gloss hiding (scale)
import Graphics.Gloss.Interface.IO.Interact hiding (scale)

import Debug.Trace


-- | InputHandler is responsible for all inputs that happen
--   It contains three cases, each explained in further detail below
inputHandler :: Event -> World -> IO World

-- | This is a case involving an action that happens immediately once.
--   Such as selecting a point for which to generate the Julia:
--   'lmb' selects the current mouse position as new offset point
inputHandler (EventKey (MouseButton LeftButton) Down _ (x, y)) w =
  let newData = (gData w) { offset = (x / 125, y / 125)}
   in return w { gData     = newData
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



-- | Handles a single step in our world
--   We compute and render the fractal in here,
--   only if our boolean flag is set to True
--   When reading the step-by step at the side, please be mindful that this happens
--   in inverse order: From bottom to top
stepHandler :: Float -> World -> IO World
stepHandler _ w@(MkWorld screen d tf _ True) = do
  putStrLn "Rendering... Please stand by"
  let picture = draw screen          -- turned into a pretty picture 'v'
              . getColors colorList  -- turned into colored grid    :: Grid Color
              . getEscapeSteps       -- turned into numbered grid   :: Grid Int
              . getSequences d       -- turned into sequenced grid  :: Grid [Point]
              . (`scale` tf)         -- Scaled to our parameters    :: Grid Point
              $ screen               -- The unscaled default screen :: Grid Point
  return $ w { currentPicture = picture
             , isChanged      = False }

-- | Default case - nothing is changed
stepHandler _ w = return w


-- | Perform linear transformation with given zooming scale, r offset, i offset.
scale :: Grid Point -> (ZoomScale, Translation) -> Grid Point
scale grid (zoom, (rOff, iOff)) = gridMap f grid
  where
    f :: Point -> Point
    f (r, i) = (r * zoom' + rOff, i * zoom' + iOff)
    zoom' = zoom * scaleFactor
