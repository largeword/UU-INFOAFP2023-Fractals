{-
  Simple bouncing balls animation using Gloss
  Pedro Vasconcelos, 2018
-}
module Main where

import System.Random
import Control.Monad (forM)

import Graphics.Gloss

type Ball = (Point, Vector)  -- position, velocity

-- imported from Graphics.Gloss:
-- Point = (Float,Float)
-- Vector = (Float,Float)

-- | convert a list of balls into a picture
drawBalls :: [Ball] -> Picture
drawBalls balls = pictures (zipWith drawBall colors balls)
  where
    colors = cycle [red, green, blue, yellow]
    drawBall c ((x,y), (dx,dy)) 
      = translate x y (color c (circleSolid ballRadius))

-- | advance a single simulation step;
-- dt is the elapsed time since last step
updateBalls :: Float -> [Ball] -> [Ball]
updateBalls dt balls = map (updateBall dt) balls
  
updateBall :: Float -> Ball -> Ball  
updateBall dt ((x,y),(dx,dy)) = ((x',y'), (dx',dy'))
  where (x',dx') = clip x dx (maxX-ballRadius)
        (y',dy') = clip y dy (maxY-ballRadius)
        -- clip to a bounding interval
        clip h dh max
          | h' > max = (max, -dh)
          | h' < -max= (-max, -dh)
          | otherwise = (h', dh)
          where h' = h + dt*dh
                
               
-- | constants
-- number of balls to simulate
numBalls :: Int          
numBalls = 100
          
-- | raidus of each ball (pixels)           
ballRadius :: Float
ballRadius = 10

-- | boundaries for the virtual box
maxX, maxY :: Float
maxX = 300
maxY = 300

-- | simulation rate (frames per second)
fps :: Int
fps = 60

       
-- | pick a ball with random position and velocity
-- for initializing the simulation state
randomBall :: IO Ball
randomBall = do
  x <- randomRIO (-maxX,maxX) 
  y <- randomRIO (-maxY,maxY) 
  dx <- randomRIO (-200,200) 
  dy <- randomRIO (-200,200)
  return ((x,y),(dx,dy)) 

-- | window dimensions and title
window :: Display
window = InWindow "Balls"  (2*floor maxX,2*floor maxY) (0,0)


-- | application start
main :: IO ()
main = do
  balls <- forM [1..numBalls] (\_ -> randomBall)
  simulate window black fps balls drawBalls (\_ -> updateBalls)
