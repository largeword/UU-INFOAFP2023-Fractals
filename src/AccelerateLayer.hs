module AccelerateLayer where

import Data.Array.Accelerate              as A
import Data.Array.Accelerate.LLVM.Native  as CPU
import Data.Array.Accelerate.LLVM.PTX     as GPU
import Data.Array.Accelerate.Interpreter  as I

import Data.List.Split (chunksOf)

type Grid a = [[a]]

-- get runExp function from official docs, for debugging
runExp :: Elt e => Exp e -> e
runExp e = indexArray (CPU.run (unit e)) Z

grid2AccArrFloat :: Grid Float -> Matrix Float
grid2AccArrFloat grid = fromList (Z:.x:.y) flatList :: Matrix Float
  where x = Prelude.length grid
        y = Prelude.length (head grid)
        flatList = concat grid


accArr2GridFloat :: Matrix Float -> Grid Float
accArr2GridFloat accMtx = reshapeList flatList x
  where flatList = toList accMtx
        Z :. y :. x = runExp $ shape $ use accMtx
  
reshapeList :: [a] -> Int -> [[a]]
reshapeList xs xPixel = chunksOf xPixel xs

