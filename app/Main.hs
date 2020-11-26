module Main where
import System.Environment

import Repl
import VM.VM

main :: IO ()
main = do [f] <- getArgs
          s   <- readFile f
          readAndRun show s
          
