{-# LANGUAGE DeriveAnyClass, 
             DeriveGeneric, 
             FlexibleContexts, 
             TypeFamilies, 
             TypeOperators, 
             FlexibleContexts, 
             StandaloneDeriving, 
             UndecidableInstances, 
             AllowAmbiguousTypes #-}

module GeneratorAcc (getSequencesAcc, getEscapeStepsAcc, grid2Arr, arr2Grid) where

import Data.Array.Accelerate              as A
import Data.Array.Accelerate.LLVM.Native  as CPU
import Data.Array.Accelerate.LLVM.PTX     as GPU
import Data.Array.Accelerate.Interpreter  as I
import Data.Array.Accelerate.Smart as S
import Data.Array.Accelerate.Sugar.Elt

import Data.List.Split (chunksOf)

import ModelAcc


-- let mat1 = fromList (Z:.50:.50) [0,0.001 ..]
-- let mat2 = fromList (Z:.50:.50) [0,0.001 ..]
-- mat = A.zip (use mat1) (use mat2)
-- a = CPU.run $ getEscapeStepsAcc $ getSequencesAcc2 mat


-- let mat = fromList (Z:.4:.2) [(1,2),(101,200),(1,2),(100,2),(2,3),(100,30),(1,2),(3,3)]


-- let mat = fromList (Z:.4:.2) [(1,2),(101,200),(1,2),(100,2),(2,3),(100,30),(1,2),(3,3)]
-- A.replicate (A.constant (Z :. All :. All :. (4::Int))) (use mat)
{-
iterateExpr :: GeneratorData -> Exp (Float, Float) -> Exp (Float, Float)
iterateExpr genData point = lift (zx A.** 2 A.- zy A.** 2 + 0, 2 A.* zx A.* zy + 0)
                    where -- point = A.snd idxPt
                          zx = A.fst point
                          zy = A.snd point
                          fracFunc = func     genData 
                          c        = offset   genData
                          z        = position genData
-}                          



getSequencesAcc :: GeneratorData -> Acc (Array ((Z :. Int) :. Int) (Float, Float)) -> Acc (Array (((Z :. Int) :. Int) :. Int) (Float, Float))
getSequencesAcc genData gridAcc = A.map (getValueOnStepAcc genData) gridAcc''
                          where gridAcc' = A.replicate (A.constant (Z :. All :. All :. (100::Int))) gridAcc
                                gridAcc'' = A.indexed gridAcc'
                                -- gridShape = shape gridAcc'
                                -- gridAccFlat = A.flatten gridAcc''

iterateExpr :: GeneratorData -> Exp (Float, Float) -> Exp (Float, Float)
iterateExpr genData = fracFunc c
                      where -- point = A.snd idxPt
                            fracFunc = func     genData 
                            c        = offset   genData
                            z        = position genData

getValueOnStepAcc :: GeneratorData -> Exp (((Z :. Int) :. Int) :. Int, (Float, Float)) -> Exp (Float, Float)
getValueOnStepAcc genData idxWithPoint = A.ifThenElse interateFlag
                                                      (A.iterate (lift t) (iterateExpr genData {position = point}) point)
                                                      (A.iterate (lift t) (iterateExpr genData {offset = point}) point)
                               where idx = A.fst idxWithPoint
                                     (T3 x y t) = A.unindex3 idx  
                                     point = A.snd idxWithPoint
                                     
                                     interateFlag = A.lift (parameter genData Prelude.== VarZ) :: Exp Bool





-- let mat = fromList (Z:.2:.2:.2) [(1,2),(101,200),(1,2),(100,2),(2,3),(100,30),(1,2),(3,3)]
-- CPU.run $ getEscapeStepsAcc $ use mat
-- Matrix (Z :. 2 :. 2) 
--  [ 1, 1,
--    1, 2]
getEscapeStepsAcc :: Acc (Array (((Z :. Int) :. Int) :. Int) (Float, Float)) -> Acc (Array ((Z :. Int) :. Int) Int)
getEscapeStepsAcc gridAcc = A.asnd $ (A.filter (A.== (lift True)) (A.map crossThreshouldAcc gridAcc))


-- let mat = fromList (Z:.2:.2:.2) [(1,2),(101,200),(1,2),(100,2),(2,3),(100,30),(1,2),(23,45)]
-- CPU.run $ A.map crossThreshouldAcc $ use mat
crossThreshouldAcc :: Exp (Float, Float) -> Exp Bool
crossThreshouldAcc point = A.ifThenElse ((A.isNaN zx) A.|| (A.isNaN zy)) 
                              (lift False) 
                              ((zx A.** 2 + zy A.** 2) A.< 100)
                              where --(zx', zy') = unlift pointPair
                                    zx = A.fst point
                                    zy = A.snd point


-- source: https://hackage.haskell.org/package/accelerate-1.3.0.0/docs/src/Data.Array.Accelerate.Language.html
-- get runExp function from Accelerate docs
-- for getting shape from Array
runExp :: Elt e => Exp e -> e
runExp e = indexArray (CPU.run (unit e)) Z

grid2Arr :: (Elt a) => Grid a -> Matrix a
grid2Arr grid = fromList (Z:.x:.y) flatList
  where x = Prelude.length grid
        y = Prelude.length (head grid)
        flatList = concat grid

arr2Grid :: (Elt a) => Matrix a -> Grid a
arr2Grid accMtx = reshapeList flatList x
  where flatList = toList accMtx
        Z :. y :. x = runExp $ shape $ use accMtx

-- reshape a list
reshapeList :: [a] -> Int -> [[a]]
reshapeList xs xPixel = chunksOf xPixel xs

