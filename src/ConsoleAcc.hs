module ConsoleAcc where

import ModelAcc

import Text.Read (readMaybe)

import Data.Array.Accelerate              as A (lift)


getGenData :: IO GeneratorData
getGenData = do
    gd  <- getGeneratorBase
    gd' <- getEscapeRadius gd
    putStrLn "Initialisation of generatorData complete"
    putStrLn "Please find your fractal in the application window."
    return gd'
  
getGeneratorBase :: IO GeneratorData
getGeneratorBase = do
  putStrLn "\nWould you like to launch a default implementation? [y|n]\n" 
  
  c <- getChar
  _ <- getChar
  case c of
    'y' -> getDefaultImplementation
    'n' -> getSpecifiedImplementation
    _   -> do putStrLn "Unable to parse input"
              getGeneratorBase

getEscapeRadius :: GeneratorData -> IO GeneratorData
getEscapeRadius gd = do
  putStrLn "\nHow many escape steps would you like to count? (default 100)"
  putStrLn "The more escape steps, the higher fidelity fractals are,"
  putStrLn "but the slower they are computed\n"
  
  strAns <- getLine
  let intAns = readMaybe strAns :: Maybe Int
  case intAns of
      Nothing -> do putStrLn "Unable to parse input.  Please make sure to enter an integer value"
                    getEscapeRadius gd
      Just a  -> return gd {escapeRadius = a}
    



getSpecifiedImplementation :: IO GeneratorData
getSpecifiedImplementation = do
    varParam <- getVarParam
    absVal   <- getAbsValue
    polyDgr  <- getPolyDegree

    let fracFunc = makeFractalFunctionAcc absVal polyDgr
        genData  = GenData { position     = A.lift (0 :: Float, 0 :: Float)
                           , offset       = A.lift (0 :: Float, 0 :: Float)
                           , escapeRadius = undefined
                           , parameter    = varParam
                           , func         = makeFractalFunctionAcc absVal polyDgr}

    return genData

  where
    getVarParam :: IO VarParameter
    getVarParam = do
        putStrLn "\nWould you like to iterate over the position (as in the Mandelbrot set)"
        putStrLn "Or over the offset (as in the Julia set)"
        putStrLn "Or over both position and offset?"
        putStrLn "[m|j|b]\n"

        c <- getChar
        _ <- getChar
        case c of
          'm' -> return VarC
          'j' -> return VarZ
          'b' -> return VarZandC
          _   -> do putStrLn "Unable to parse input"
                    getVarParam

    getAbsValue :: IO Bool
    getAbsValue = do
        putStrLn "\nWould you like to iterate using the absolute value of the polynomial"
        putStrLn "(as in the Burning Ship fractals)?"
        putStrLn "[y|n]\n"

        c <- getChar
        _ <- getChar
        case c of
          'y' -> return True
          'n' -> return False
          _   -> do putStrLn "Unable to parse input"
                    getAbsValue

    getPolyDegree :: IO Int
    getPolyDegree = do
        putStrLn "\nWhat degree of polynomial would you like to use? (default 2 - squared)"
        putStrLn "Keep in mind that a higher polynomial degree will take longer to compute"
        putStrLn "As of now, only positive integer polynomials > 0 are supported\n"

        strAns <- getLine
        let intAns = readMaybe strAns :: Maybe Int
        case intAns of
          Nothing -> do putStrLn "Unable to parse input.  Please make sure to enter an integer value"
                        getPolyDegree
          Just a  -> if   a > 0
                     then return a
                     else do putStrLn "As of now, only positive integer polynomials > 0 are supported"
                             getPolyDegree



getDefaultImplementation :: IO GeneratorData
getDefaultImplementation = do
    putStrLn "\nWhich of the following default implementations would you like to load?"
    putStrLn "[1] Default Mandelbrot"
    putStrLn "[2] Default Julia"
    putStrLn "[3] Burning Ship (Mandelbrot)"
    putStrLn "[4] Burning Ship (Julia)"
    putStrLn "[b] Back\n"
    
    c <- getChar -- getChar only seems to read an input after the user presses enter
    _ <- getChar -- this empty read parses the newline character and throws it away
    case c of
        '1' -> return mandelbrotDefault
        '2' -> return juliaDefault
        '3' -> return burningShipDefault
        '4' -> return burningJuliaDefault
        'b' -> getGeneratorBase
        _   -> do putStrLn "unrecognised symbol"
                  getDefaultImplementation


mandelbrotDefault :: GeneratorData
mandelbrotDefault = GenData { position     = A.lift (0 :: Float, 0 :: Float)
                            , offset       = A.lift (0 :: Float, 0 :: Float)
                            , escapeRadius = undefined
                            , parameter    = VarC
                            , func         = makeFractalFunctionAcc False 2 }

juliaDefault :: GeneratorData
juliaDefault = GenData { position     = A.lift (0 :: Float, 0 :: Float)
                       , offset       = A.lift (0 :: Float, 0 :: Float)
                       , escapeRadius = undefined
                       , parameter    = VarZ
                       , func         = makeFractalFunctionAcc False 2 }

burningShipDefault :: GeneratorData
burningShipDefault = GenData { position     = A.lift (0 :: Float, 0 :: Float)
                             , offset       = A.lift (0 :: Float, 0 :: Float)
                             , escapeRadius = undefined
                             , parameter    = VarC
                             , func         = makeFractalFunctionAcc True 2 }

burningJuliaDefault :: GeneratorData
burningJuliaDefault = GenData { position     = A.lift (0 :: Float, 0 :: Float)
                              , offset       = A.lift (0 :: Float, 0 :: Float)
                              , escapeRadius = undefined
                              , parameter    = VarZ
                              , func         = makeFractalFunctionAcc True 2 }

