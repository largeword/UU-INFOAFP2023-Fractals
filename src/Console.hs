module Console where

import Model

import Text.Read (readMaybe)


getGenData :: IO GeneratorData
getGenData = do
    gd  <- getGeneratorBase
    gd' <- getEscapeRadius gd
    putStrLn "Initialisation of generatorData complete"
    putStrLn "Please find your fractal in the application window."
    return gd'
  
  where
    getGeneratorBase :: IO GeneratorData
    getGeneratorBase = do
      putStrLn "\nWould you like to launch a default implementation? [y/n]\n" 
      
      c <- getChar
      _ <- getChar
      case c of
        'y' -> getDefaultImplementation
        'n' -> getSpecifiedImplementation

    getEscapeRadius :: GeneratorData -> IO GeneratorData
    getEscapeRadius gd = do
      putStrLn "How many escape steps would you like to count? (default 100)"
      putStrLn "The more escape steps, the higher fidelity fractals are,"
      putStrLn "but the slower they are computed"
      
      strAns <- getLine
      let intAns = readMaybe strAns :: Maybe Int
      case intAns of
          Nothing -> do putStrLn "Unable to parse input.  Are you sure it is an integer?"
                        getEscapeRadius gd
          Just a  -> return gd {escapeRadius = a}
    


{-
data GeneratorData = GenData
  { position         :: Point            -- other name for z in fractal function?
  , offset           :: Point            -- offset c in the fractal polynomial
  , escapeRadius     :: Int              -- TO BE EXPLAINED 
  , parameter        :: VarParameter 
  , func             :: FractalFunction       
  } -}

getSpecifiedImplementation :: IO GeneratorData
getSpecifiedImplementation = do
    undefined

  where
    getVarParam :: IO VarParameter
    getVarParam = do
        undefined


getDefaultImplementation :: IO GeneratorData
getDefaultImplementation = do
    putStrLn "\nWhich of the following default implementations would you like to load?"
    putStrLn "[1] Default Mandelbrot"
    putStrLn "[2] Default Julia"
    putStrLn "[3] Burning Ship (Mandelbrot)"
    putStrLn "[4] Burning Ship (Julia)\n"
    
    c <- getChar -- getChar only seems to read an input after the user presses enter
    _ <- getChar -- this empty read parses the newline character and throws it away
    case c of
        '1' -> return mandelbrotDefault
        '2' -> return juliaDefault
        '3' -> return burningShipDefault
        '4' -> return burningJuliaDefault
        _   -> do putStrLn "unrecognised symbol"
                  getDefaultImplementation



mandelbrotDefault :: GeneratorData
mandelbrotDefault = GenData { position     = (0,0)
                            , escapeRadius = 100
                            , parameter    = VarC
                            , offset       = (0,0)
                            , func         =  makeFractalFunction False 2 }

juliaDefault :: GeneratorData
juliaDefault = GenData { position     = (0,0)
                       , escapeRadius = 100
                       , parameter    = VarZ
                       , offset       = (0,0)
                       , func         =  makeFractalFunction False 2 }

burningShipDefault :: GeneratorData
burningShipDefault = GenData { position     = (0,0)
                             , escapeRadius = 100
                             , parameter    = VarC
                             , offset       = (0,0)
                             , func         =  makeFractalFunction True 2 }

burningJuliaDefault :: GeneratorData
burningJuliaDefault = GenData { position     = (0,0)
                              , escapeRadius = 100
                              , parameter    = VarZ
                              , offset       = (0,0)
                              , func         =  makeFractalFunction True 2 }

