import System.Environment
import GHC.Float (int2Float)
import qualified Generator as G
import qualified Test.Tasty.QuickCheck as QC
import Model
import View
import Controller (scale)
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
newtype CustomGrid = CustomGrid [[Float]] deriving (Eq,Show)
instance QC.Arbitrary CustomGrid where
  arbitrary = QC.sized $ \s -> do
                 n <- QC.choose (1, 10)
                 grid <- QC.vectorOf n (QC.vectorOf n (QC.choose (-100.0,100.0)))
                 return (CustomGrid grid)
  shrink (CustomGrid grid) = map CustomGrid (QC.shrink grid)

-- | This is a custom Grid Point generator
newtype CustomGridPoint = CustomGridPoint [[(Float, Float)]] deriving (Eq,Show)
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
gd = GenData { position     = (0 :: Float, 0 :: Float)
             , offset       = (0 :: Float, 0 :: Float)
             , escapeRadius = undefined
             , parameter    = VarZ
             , func         = makeFractalFunction False 2 }


-- | This property tests whether the escaping step is within a reasonable range
prop_getEscapeSteps :: CustomGridPoint -> (Float, (Float, Float)) -> CustomInt -> Bool
prop_getEscapeSteps (CustomGridPoint grid) tf (CustomInt r) = foldl (\a b -> b >= 0 && b <= escapeRadius gd' && a) 
                                                                       True 
                                                                       grid'
  where grid'        = concat escapingStep
        escapingStep = G.getEscapeSteps 
                     . G.getSequences gd' 
                     . (`scale` tf) $ grid
        gd'          = gd {escapeRadius = r}


-- | This property tests whether the mapping color value is within a reasonable range
prop_mapColorRange :: CustomGridPoint -> (Float, (Float, Float)) -> CustomInt -> Bool
prop_mapColorRange (CustomGridPoint grid) tf (CustomInt r) = foldl (\a b -> b >= 0.0 && b <= int2Float (length colorList) && a) 
                                                                   True
                                                                   grid'
  where grid'        = concat (rescaleGrid2ColorRange colorList escapingStep)
        escapingStep = G.getEscapeSteps 
                     . G.getSequences gd' 
                     . (`scale` tf) $ grid
        gd'          = gd {escapeRadius = r}


tests :: TestTree
tests = testGroup "Tested by QuickCheck"
  [ QC.testProperty "Escaping Step within Range" prop_getEscapeSteps
  , QC.testProperty "Color Index within Range" prop_mapColorRange]
