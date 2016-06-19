module Utils where

import System.Exit

abort msg = do putStrLn msg
               exitFailure

exit msg = do putStrLn msg
              exitSuccess