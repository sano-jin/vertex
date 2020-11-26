-- | A Module for testing
-- give as `stack test --test-arguments "sample.dhl"`

import System.Environment

import Repl
import VM.VM

main :: IO ()
main = do [f] <- getArgs
          s   <- readFile f
          readAndRun showStateForDebugging s
