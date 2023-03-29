module Console where

import Model

getGenData :: IO GeneratorData
getGenData = do
    putStrLn "\nWould you like to launch a default implementation? [y/n]\n" 
    
    c <- getChar
    _ <- getChar
    case c of
        'y' -> getDefaultImplementation
        'n' -> getSpecifiedImplementation


getSpecifiedImplementation :: IO GeneratorData
getSpecifiedImplementation = undefined


getDefaultImplementation :: IO GeneratorData
getDefaultImplementation = do
    putStrLn "\nWhich of the following default implementations would you like to load?"
    putStrLn "[1] Default Mandelbrot"
    putStrLn "[2] Default Julia"
    putStrLn "[3] Burning Ship (Mandelbrot)"
    putStrLn "[4] Burning Ship (Julia)\n"
    
    c <- getChar
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

