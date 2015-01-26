module Utils where

import System.Exit

abort msg = do putStrLn msg
               exitFailure