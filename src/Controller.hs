module Controller where

import Model
import View
import Generator

import Data.List
import Data.Ord hiding (Down)

import Graphics.Gloss hiding (scale)
import Graphics.Gloss.Interface.IO.Interact hiding (scale)

import Debug.Trace


-- | InputHandler is responsible for all inputs that happen
--   It contains three cases, each explained in further detail below
inputHandler :: Event -> World -> World

-- | This is a special (and WIP) case.  It involves an action that happens
--   immediately once.
--   Such as selecting a point for which to generate the Julia
--   or (potentially in the future) using the mousewheel to zoom
inputHandler (EventKey (MouseButton LeftButton) Down _ (x, y)) w = 
  let newData = GenData (x / 125, y / 125)
   in w {gData = newData, isChanged = True}
inputHandler (EventKey (Char        'r'       ) Down _ _     ) w = w {isChanged = True}

-- | This is a case for events that happen every tick, as long as a button is held
--   Such as moving using the buttons
--   The first function of this case matches on a button pressed *Down*,
--   in which case it is added to the event list
inputHandler e@(EventKey _ Down _ _) w =
  case parseEvent e of 
    Nothing -> w
    Just ev -> let tf = doEvent ev (transform w)
                in w { transform = tf
                     , isChanged = True }

-- | Lastly, wildcard pattern returns the input world
inputHandler _ w = w


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



-- | Handles a single step in our world
--   We compute and render the fractal in here,
--   only if our boolean flag is set to True
stepHandler :: Float -> World -> World
stepHandler _ w@(MkWorld screen d tf _ True) =
  let picture  = draw screen                        -- turned into a pretty picture 'v'
               . getColors colorList                -- turned into colored grid    :: Grid Color
               . getEscapeSteps (escapeRadius d)    -- turned into numbered grid   :: Grid Int
               . getSequences d                     -- turned into sequenced grid  :: Grid [Point]
               . (`scale` tf)                       -- Scaled to our parameters    :: Grid Point
               $ screen                             -- The unscaled default screen :: Grid Point
   in w { currentPicture = picture
        , isChanged      = False }

-- | Default case - nothing is changed
stepHandler _ w = w


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



-- KEY MAPPINGS

-- Panning: translation magnitudes (eventually within window app?)

    -- in x direction

    -- in y direction

-- Zoomscaling

-- Later on: loading config document or querying user for input values via console (implement in main.hs in do-block)