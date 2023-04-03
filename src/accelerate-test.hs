-- modified based on Accelerate official example, get it from:
-- https://www.acceleratehs.org/get-started.html

{-# LANGUAGE FlexibleContexts #-}

import Data.Array.Accelerate              as A
import Data.Array.Accelerate.LLVM.Native  as CPU
import Data.Array.Accelerate.LLVM.PTX     as GPU

dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp xs ys = A.fold (**) 0.5 (A.zipWith (*) xs ys)
