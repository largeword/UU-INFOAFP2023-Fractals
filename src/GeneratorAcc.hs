{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleContexts, TypeFamilies, TypeOperators, FlexibleContexts #-}



module GeneratorAcc (getSequencesAcc2, getEscapeStepsAcc, accArr2Grid) where

import Data.Array.Accelerate              as A
import Data.Array.Accelerate.LLVM.Native  as CPU
import Data.Array.Accelerate.LLVM.PTX     as GPU
import Data.Array.Accelerate.Interpreter  as I
import Data.Array.Accelerate.Smart as S
import Data.Array.Accelerate.Sugar.Elt

import Data.List.Split (chunksOf)

type Grid a = [[a]]

{-
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.List
import Data.List.Split
import Data.Function

import Model

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
getSequences :: GeneratorData -> Grid Point -> Grid [Point]
getSequences genData grid = gridMap ( take (escapeRadius genData) -- limit the infinite sequence
                                    . generateFractal genData     -- generate infinite sequence
                                    ) grid                        -- the grid of points


-- | Given a sequenced grid
--   compute the steps in which the point escapes the treshold
getEscapeSteps :: Grid [Point] -> Grid Int
getEscapeSteps grid = gridMap ( length              -- the amount of unescaped values
                              . filter (== True)    -- discard all escaped values
                              . map crossThreshold  -- convert the sequence to bools
                              ) grid                -- the sequenced grid 
  where
    -- | Given a point, calculate whether it's close enough to the fractal interior
    --   zx or zy can also be either NaN or Infinity
    crossThreshold :: Point -> Bool
    crossThreshold (zx, zy) = if (isNaN zx) || (isNaN zy) then False
                              else (zx ** 2 + zy ** 2) < 100



-}

-- get runExp function from official docs, for debugging
runExp :: Elt e => Exp e -> e
runExp e = indexArray (CPU.run (unit e)) Z


data Point = Point_ Float Float
  deriving (Generic, Elt)
-- let mat = fromList (Z:.2:.2) [0..]
-- CPU.run $ gridMapAcc (+1) $ use mat
gridMapAcc :: (Shape sh, Elt b, Elt a) => (Exp a -> Exp b) -> Acc (Array sh a) -> Acc (Array sh b)
gridMapAcc f gridAcc = reshape sh (A.map f (flatten gridAcc))
                       where sh = shape gridAcc

-- | Given a scaled grid, compute the sequences of iterations
-- getSequences :: GeneratorData -> Grid Point -> Grid [Point]
-- getSequences genData grid = gridMap ( take (escapeRadius genData) -- limit the infinite sequence
--                                     . generateFractal genData     -- generate infinite sequence
--                                     ) grid                        -- the grid of points



-- let mat1 = fromList (Z:.50:.50) [0,0.001 ..]
-- let mat2 = fromList (Z:.50:.50) [0,0.001 ..]
-- mat = A.zip (use mat1) (use mat2)
-- a = CPU.run $ getEscapeStepsAcc $ getSequencesAcc2 mat


-- Grid Point -> Grid [Point]

-- let mat = fromList (Z:.4:.2) [(1,2),(101,200),(1,2),(100,2),(2,3),(100,30),(1,2),(3,3)]

{-
getSequencesAcc :: Acc (Array ((Z :. Int) :. Int) (Float, Float)) -> Acc (Array (((Z :. Int) :. Int) :. Int) (Float, Float))
getSequencesAcc gridAcc = getSequencesAcc' gridAcc' 1
                          where -- gridAcc' = reshape (lift (Z :. (500::Int) :. (500::Int) :. (1::Int))) gridAcc
                                gridAcc' = A.replicate (A.constant (Z :. All :. All :. (1::Int))) gridAcc
-}

getSequencesAcc2 :: Acc (Array ((Z :. Int) :. Int) (Float, Float)) -> Acc (Array (((Z :. Int) :. Int) :. Int) (Float, Float))
getSequencesAcc2 gridAcc = A.map getValueOnStep2 gridAcc''
                          where -- gridAcc' = reshape (lift (Z :. (500::Int) :. (500::Int) :. (1::Int))) gridAcc
                                gridAcc' = A.replicate (A.constant (Z :. All :. All :. (100::Int))) gridAcc
                                gridAcc'' = A.indexed gridAcc'
                                gridShape = shape gridAcc'
                                -- gridAccFlat = A.flatten gridAcc''


getValueOnStep2 :: Exp (((Z :. Int) :. Int) :. Int, (Float, Float)) -> Exp (Float, Float)
getValueOnStep2 idxWithPoint = A.iterate (lift t) iterateExpr point
                               where idx = A.fst idxWithPoint
                                     (T3 x y t) = A.unindex3 idx  
                                     point = A.snd idxWithPoint


getSequencesAcc' :: Acc (Array (((Z :. Int) :. Int) :. Int) (Float, Float)) -> Int -> Acc (Array (((Z :. Int) :. Int) :. Int) (Float, Float))
getSequencesAcc' gridAcc n = ifThenElse ((lift n) A.< 100)
                             ((getValueOnStep gridAcc n) A.++ (getSequencesAcc' gridAcc (n+1)))
                             (getValueOnStep gridAcc n)


-- let mat = fromList (Z:.4:.2) [(1,2),(101,200),(1,2),(100,2),(2,3),(100,30),(1,2),(3,3)]
-- A.replicate (A.constant (Z :. All :. All :. (4::Int))) (use mat)
iterateExpr :: Exp (Float, Float) -> Exp (Float, Float)
iterateExpr point = lift (zx A.** 2 A.- zy A.** 2 + 0, 2 A.* zx A.* zy + 0)
                    where -- point = A.snd idxPt
                          zx = A.fst point
                          zy = A.snd point

getValueOnStep gridAcc n = A.map (A.iterate (lift n) iterateExpr) gridAcc

-- let mat = fromList (Z:.2:.2:.2) [(1,2),(101,200),(1,2),(100,2),(2,3),(100,30),(1,2),(3,3)]
-- CPU.run $ getEscapeStepsAcc $ use mat
-- Matrix (Z :. 2 :. 2) 
--  [ 1, 1,
--    1, 2]
getEscapeStepsAcc :: Acc (Array (((Z :. Int) :. Int) :. Int) (Float, Float)) -> Acc (Array ((Z :. Int) :. Int) Int)
getEscapeStepsAcc gridAcc = A.asnd $ (A.filter (A.== (lift True)) (A.map crossThreshouldAcc gridAcc))
-- getEscapeStepsAcc gridAcc = A.snd $ (A.filter (A.== (lift True)) (A.map crossThreshouldAcc gridAcc))

-- let mat = fromList (Z:.2:.2:.2) [(1,2),(101,200),(1,2),(100,2),(2,3),(100,30),(1,2),(23,45)]
-- CPU.run $ A.map crossThreshouldAcc $ use mat
crossThreshouldAcc :: Exp (Float, Float) -> Exp Bool
crossThreshouldAcc point = ifThenElse ((A.isNaN zx) A.|| (A.isNaN zy)) 
                              (lift False) 
                              ((zx A.** 2 + zy A.** 2) A.< 100)
                              where --(zx', zy') = unlift pointPair
                                    zx = A.fst point
                                    zy = A.snd point


accArr2Grid :: (Elt a) => Matrix a -> Grid a
accArr2GridInt accMtx = reshapeList flatList x
  where flatList = toList accMtx
        Z :. y :. x = runExp $ shape $ use accMtx
  
reshapeList :: [a] -> Int -> [[a]]
reshapeList xs xPixel = chunksOf xPixel xs

