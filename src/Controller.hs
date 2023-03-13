module Controller where

import Model
import View
import Generator

import Data.List

import Graphics.Gloss hiding (scale)
import Graphics.Gloss.Interface.IO.Interact hiding (scale)

import Debug.Trace
import Data.List
import Data.Ord


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

-- | This is a case for events that happen every tick, as long as a button is held
--   Such as moving using the buttons
--   The first function of this case matches on a button pressed *Down*,
--   in which case it is added to the event list
inputHandler e@(EventKey _ Down _ _) w =
  case parseEvent e of 
    Nothing -> w
    Just ev -> w {inputEvents = ev : inputEvents w}

-- | The second case matches on a button let *Up*,
--   in which case we remove the event from the list
inputHandler e@(EventKey _ Up   _ _) w = 
  case parseEvent e of
    Nothing -> w
    Just ev -> w {inputEvents = delete ev $ inputEvents w}

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
stepHandler _ w@(MkWorld screen d es z t _ True) = trace "Rendering... please hold..." $
    let (z', t') = eventStep es (z, t)
        picture  = draw screen          -- turned into a pretty picture 'v'
                 . getColors colorList  -- turned into colored grid    :: Grid Color
                 . getEscapeSteps 100   -- turned into numbered grid   :: Grid Int
                 . getSequences d       -- turned into sequenced grid  :: Grid [Point]
                 . (`scale` (z', t'))   -- Scaled to our parameters    :: Grid Point
                 $ screen               -- The unscaled default screen :: Grid Point
     in w { zoomScaling = z'
          , translation = t'
          , currentPicture = picture
          , isChanged = not $ null es }   -- whether there are still event actions

-- | Default case - nothing is changed
stepHandler _ w = trace (show (inputEvents w)) $ w


eventStep :: [EventAction] -> (Float, (Float, Float)) -> (Float, (Float, Float))
eventStep es transf = foldr doEvent transf es
  where
    doEvent :: EventAction -> (Float, (Float, Float)) -> (Float, (Float, Float))
    doEvent (Move Up'   ) (z, (tx, ty)) = (z     , (tx         , ty + 50 / z))
    doEvent (Move Left' ) (z, (tx, ty)) = (z     , (tx - 50 / z, ty         ))
    doEvent (Move Down' ) (z, (tx, ty)) = (z     , (tx         , ty - 50 / z))
    doEvent (Move Right') (z, (tx, ty)) = (z     , (tx + 50 / z, ty         ))
    doEvent (Zoom In    ) (z, (tx, ty)) = (z + 50, (tx         , ty         ))
    doEvent (Zoom Out   ) (z, (tx, ty)) = (z - 50, (tx         , ty         ))
    



-- | Perform linear transformation with given zooming scale, r offset, i offset.
scale :: Grid Point -> (Float, (Float, Float)) -> Grid Point
scale grid (zoom, (rOff, iOff)) = gridMap f grid
  where
    f :: Point -> Point
    f (r, i) = (r * zoom + rOff, i * zoom + iOff)

{- The following expression is used to get Min and Max for Grid Point
xOldMin = minimumBy (comparing (!!0)) grid
xOldMax = maximumBy (comparing (!!0)) grid
yOldMin = minimumBy (comparing (!!1)) grid
yOldMax = maximumBy (comparing (!!1)) grid
-}




-- KEY MAPPINGS

-- Panning: translation magnitudes (eventually within window app?)

    -- in x direction

    -- in y direction

-- Zoomscaling

-- Later on: loading config document or querying user for input values via console (implement in main.hs in do-block)