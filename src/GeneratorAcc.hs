{-# LANGUAGE FlexibleContexts
           , TypeFamilies
           , TypeOperators
           , UndecidableInstances
           , AllowAmbiguousTypes #-}

module GeneratorAcc (getSequencesAcc, getEscapeStepsAcc, grid2Arr, arr2Grid) where

import ModelAcc

import Data.List.Split (chunksOf)

import Data.Array.Accelerate              as A
import Data.Array.Accelerate.LLVM.Native  as CPU
import Data.Array.Accelerate.LLVM.PTX     as GPU

import Prelude as P


-- | Computes the list of sequenced applications of our fractal function over the grid
--   We create a replicated indexed cubic grid the size of the escape radius
--   and apply iterations over this map
getSequencesAcc :: GeneratorData -> Acc (Matrix (Float, Float)) -> Acc (Cubic (Float, Float))
getSequencesAcc genData gridAcc = A.map (getValueOnStepAcc genData) gridAcc''
  where 
    gridAcc'  = A.replicate (A.constant (Z :. All :. All :. (t::Int))) gridAcc
    gridAcc'' = A.indexed gridAcc'
    t         = escapeRadius genData

-- | Iterate the fracFunc over a certain index
--   The third dimension of the index, t, defines how much we interate
getValueOnStepAcc :: GeneratorData -> Exp (((Z :. Int) :. Int) :. Int, (Float, Float)) -> Exp (Float, Float)
getValueOnStepAcc genData idxWithPoint = A.iterate (lift t) (`fracFunc` c) z 
  where 
    idx        = A.fst idxWithPoint
    (T3 x y t) = A.unindex3 idx  
    point      = A.snd idxWithPoint
    genData'   = case parameter genData of 
                   VarC -> genData {position = point} 
                   VarZ -> genData {offset = point}
    fracFunc   = func genData'
    z          = offset genData'
    c          = position genData' 
    

-- | Filter a cubic into a matrix by checking how many of the points cross the treshold
getEscapeStepsAcc :: Acc (Cubic (Float, Float)) -> Acc (Matrix Int)
getEscapeStepsAcc gridAcc = A.asnd $ A.filter (A.== lift True) (A.map crossThreshouldAcc gridAcc)

-- | Check whether a point crosses the threshold
--   Currently this is set to 10
--   Reducing the square root from the computation, we perform it with 10^2: 100
crossThreshouldAcc :: Exp (Float, Float) -> Exp Bool
crossThreshouldAcc point = A.ifThenElse (A.isNaN zx A.|| A.isNaN zy)
                                        (lift False) 
                                        ((zx A.** 2 + zy A.** 2) A.< 100)
  where
    zx = A.fst point
    zy = A.snd point


-- source: https://hackage.haskell.org/package/accelerate-1.3.0.0/docs/src/Data.Array.Accelerate.Language.html
-- get runExp function from Accelerate docs
-- for getting shape from Array
runExp :: Elt e => Exp e -> e
runExp e = indexArray (CPU.run (unit e)) Z

grid2Arr :: (Elt a) => Grid a -> Matrix a
grid2Arr grid = fromList (Z:.x:.y) flatList
  where x = P.length grid
        y = P.length (head grid)
        flatList = concat grid

arr2Grid :: (Elt a) => Matrix a -> Grid a
arr2Grid accMtx = reshapeList flatList x
  where flatList = toList accMtx
        Z :. y :. x = runExp $ shape $ use accMtx

-- reshape a list
reshapeList :: [a] -> Int -> [[a]]
reshapeList xs xPixel = chunksOf xPixel xs

