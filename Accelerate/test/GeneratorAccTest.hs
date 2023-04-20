import System.Environment
import qualified GeneratorAcc as GA
import qualified Test.Tasty.QuickCheck as QC
import qualified Data.Array.Accelerate              as A (use, lift)
import qualified Data.Array.Accelerate.LLVM.Native  as CPU (run)
import ModelAcc
import ControllerAcc (scaleAcc)
import Test.Tasty (defaultMain, testGroup, TestTree)


main = do
  -- set runtime parameters for Tasty
  setEnv "TASTY_NUM_THREADS" "8"
  setEnv "TASTY_QUICKCHECK_TESTS" "100"
  setEnv "TASTY_QUICKCHECK_VERBOSE" "TRUE"
  setEnv "TASTY_QUICKCHECK_SHRINKS" "0"
  
-- do tests
  defaultMain tests

-- | This is a custom Grid Float generator
newtype CustomGrid = CustomGrid (Grid Float) deriving (Eq,Show)
instance QC.Arbitrary CustomGrid where
  arbitrary = QC.sized $ \s -> do
                 n <- QC.choose (1, 10)
                 grid <- QC.vectorOf n (QC.vectorOf n (QC.choose (-100.0,100.0)))
                 return (CustomGrid grid)
  shrink (CustomGrid grid) = map CustomGrid (QC.shrink grid)

-- | This is a custom Grid Point generator
newtype CustomGridPoint = CustomGridPoint (Grid (Float, Float)) deriving (Eq,Show)
instance QC.Arbitrary CustomGridPoint where
  arbitrary = QC.sized $ \s -> do
                 n <- QC.choose (1, 10)
                 grid <- QC.vectorOf n (QC.vectorOf n (QC.choose ((-100.0, -100.0),(100.0, 100.0))))
                 return (CustomGridPoint grid)
  shrink (CustomGridPoint grid) = map CustomGridPoint (QC.shrink grid)

-- | This is a custom escapeRadius generator
newtype CustomInt = CustomInt Int deriving (Eq,Show)
instance QC.Arbitrary CustomInt where
  arbitrary = QC.sized $ \s -> do
                 x <- QC.choose (5,100)
                 return (CustomInt x)

-- | Pre-define GenData, hard to make it happy with geneerator from QC
gd = GenData { position     = A.lift (0 :: Float, 0 :: Float)
             , offset       = A.lift (0 :: Float, 0 :: Float)
             , escapeRadius = undefined
             , parameter    = VarZ
             , func         = makeFractalFunctionAcc False 2 }


-- | This property tests the correctness of GA.grid2Arr and GA.arr2Grid
prop_grid2Arr2Grid :: CustomGrid -> Bool
prop_grid2Arr2Grid (CustomGrid grid) = grid == GA.arr2Grid (GA.grid2Arr grid)


-- | This property tests whether the escaping step is within a reasonable range
prop_getEscapeStepsAcc :: CustomGridPoint -> (Float, (Float, Float)) -> CustomInt -> Bool
prop_getEscapeStepsAcc (CustomGridPoint grid) tf (CustomInt r) = foldl (\a b -> b >= 0 && b <= escapeRadius gd' && a) 
                                                                       True 
                                                                       grid'
  where gd' = gd {escapeRadius = r}
        grid' = concat (GA.arr2Grid escapingStep)
        escapingStep = CPU.run $ GA.getEscapeStepsAcc 
                     . GA.getSequencesAcc gd' 
                     . (`scaleAcc` tf) $ A.use (GA.grid2Arr grid)


tests :: TestTree
tests = testGroup "Tested by QuickCheck"
  [ QC.testProperty "Grid -> Arr -> Grid" prop_grid2Arr2Grid
  , QC.testProperty "Escaping Step within Range" prop_getEscapeStepsAcc]

