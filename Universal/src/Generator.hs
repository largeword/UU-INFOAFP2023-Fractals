module Generator (getSequences, getEscapeSteps) where

import Model

import Data.List
import Data.Function

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact


-- | Computes a fractal pattern based on fractal meta data and a given point
generateFractal :: GeneratorData -> Point -> [Point]
generateFractal genData pt = case parameter genData of
  VarZ -> computeFractal genData {position = pt}
  VarC -> computeFractal genData {offset   = pt}
  _    -> error "Other functionalities are not yet defined."

-- | Generates the infinite fractal set, defining the iterations
--   according to the generation data provided
computeFractal :: GeneratorData -> [Point]
computeFractal genData = let fracFunc = func     genData
                             c        = offset   genData
                             z        = position genData
                         in
                           iterate (`fracFunc` c) z


-- | Given a scaled grid, compute the sequences of iterations
--   Note that the grid is eta-reduced
getSequences :: GeneratorData -> Grid Point -> Grid [Point]
getSequences genData = gridMap ( take (escapeRadius genData)  -- limit the infinite sequence
                               . generateFractal genData)     -- generate infinite sequence


-- | Given a sequenced grid
--   compute the steps in which the point escapes the treshold
--   Note that the grid is eta-reduced
getEscapeSteps :: Grid [Point] -> Grid Int
getEscapeSteps = gridMap ( length               -- the amount of unescaped values
                         . filter (== True)     -- discard all escaped values
                         . map crossThreshold)  -- convert the sequence to bools
  where
    -- | Given a point, calculate whether it's close enough to the fractal interior
    --   zx or zy can also be either NaN or Infinity
    crossThreshold :: Point -> Bool
    crossThreshold (zx, zy) = not (isNaN zx || isNaN zy) && (zx ** 2 + zy ** 2) < 100
