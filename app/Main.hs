module Main where
import           System.Environment

import           Repl                           ( readAndRun )
import           ND                             ( readAndRunND
                                                , showEnds
                                                , showAllStates
                                                ) 
import           VM.VM                          ( showStateForDebugging )
import qualified Data.Set                       as S


selector :: S.Set String -> String -> a -> a -> a
selector argsSet argName f g
  = if S.member argName argsSet then f else g

main :: IO ()
main = do
  (f : args) <- getArgs
  s          <- readFile f
  let argsSet  = S.fromList args
      select   = selector argsSet 
      dumper   = select "--dump" showStateForDebugging show
      executor =
        select "--nd"
        (readAndRunND $ select "--show-all" showAllStates showEnds)
        readAndRun
      unexpectedArgs =
        S.difference argsSet
        $ S.fromList ["--dump", "--nd", "--show-all"]
    in
      if S.null unexpectedArgs
      then executor dumper s
      else putStrLn $ "unexpected arg(s) " ++ show (S.toList unexpectedArgs)
