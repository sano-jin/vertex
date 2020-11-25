module Main where
import System.Environment

import Repl

main :: IO ()
main = do [f] <- getArgs
          s   <- readFile f
          readAndRun s
          
