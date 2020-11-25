module Main where
import System.Environment

import Repl

main :: IO ()
main = do args <- getArgs
          readAndRun (head args)
          
