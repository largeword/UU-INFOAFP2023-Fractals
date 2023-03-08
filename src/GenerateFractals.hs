module GenerateFractals where


import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact


-- complex number iteration
next :: Point -> Point -> Point
next (u, v) (x, y) = (x * x - y * y + u, 2 * x * y + v)


-- generate infinite sequence of given point
mandelbrot :: Point -> [Point]
mandelbrot p = iterate (next p) (0, 0)  -- z equals to origin (0,0)

julia :: Point -> Point -> [Point]
julia c = iterate (next c)  -- z is not fixed


-- if the square distance <100, than they are closed
fairlyClose :: Point -> Bool
fairlyClose (u, v) = (u * u + v * v) < 100

{-
-- only take fist n items to check if they are closed to p
approxTest :: Int -> Point -> (Point -> [Point]) -> Bool
approxTest n p fractalIterate = foldr (&&) True $ fmap fairlyClose (take n (fractalIterate p))
-}

-- choose the color for given point according to the ealiest non-close moment
chooseColor :: [a] -> [Point] -> a
chooseColor palette = (palette !!) . length . take n . takeWhile fairlyClose
                      where n = length palette - 1

                      
type Image a = Point -> a

-- coloring the given point according to palette
fracImage :: (Point -> [Point]) -> [a] -> Image a
fracImage sequenceIterator palette = chooseColor palette . sequenceIterator


type Grid a = [[a]]

-- find corresponding point for each pixel
grid :: Int -> Int -> Point -> Point -> Grid Point
-- c: colums of pixels
-- r: rows of pixels
-- (xmin, ymin): point at the bottom left corner 
-- (xmax, ymax): point at the top right corner
grid c r (xmin, ymin) (xmax, ymax) = [[(x, y) | x <- for c xmin xmax] | y <- for r ymin ymax]

-- generate middle number with given range and count
for :: Int -> Float -> Float -> [Float]
for n min max = take n [min, min + delta..]
                where delta = (max - min) / fromIntegral (n - 1)


-- coloring grid point
sample :: Grid Point -> Image a -> Grid a
sample pixels coloring = map (map coloring) pixels

-- taking a image render then giving a image typed b
draw :: Grid Point -> (Point -> [Point]) -> [a] -> (Grid a -> b) -> b
draw pixels sequenceIterator palette render = render (sample pixels (fracImage sequenceIterator palette))


-- Character-based Pictures of the Mandelbrot Set
charPalette :: [Char]
charPalette = " ,.`\"~:;o-!|?/<>X+={^O#%&@8*$"

charRender :: Grid Char -> IO ()
charRender = putStr . unlines

figure1 = draw points mandelbrot charPalette charRender 
          where points = grid 79 37 (-2.25, -1.5) (0.75, 1.5)

figure2 = draw points (julia (0.32, 0.043)) charPalette charRender
          where points = grid 79 37 (-1.5, -1.5) (1.5, 1.5)


-- make color list
makeColorList :: [Int] -> [Color]
makeColorList xs = [makeColorI y y y 255 | y <- xs]

gradien :: [Int]
gradien = [0..255]

greyList :: [Color]
greyList = makeColorList gradien
