module Main where
import           System.Environment

import           Repl
-- import           VM.VM

main :: IO ()
main = do
  (f : args) <- getArgs
  s          <- readFile f
  case args of
    ["--nd"] -> readAndRunND show s
    _        -> readAndRun show s

