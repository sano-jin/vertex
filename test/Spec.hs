-- module Main where
-- | A Module for testing
--   give as `stack test --test-arguments "sample.dhl"`

import           System.Environment
import           Repl
import           ND
import           VM.VM


main :: IO ()
main = do (f:args) <- getArgs
          s   <- readFile f
          case args of
            ["--nd"]
              -> readAndRunND showStateForDebugging s
            _
              -> readAndRun showStateForDebugging s

