module Repl where
import System.Environment
import System.IO
import Compiler.Compiler (
  compile,
  showProcs,
  showRule
  )
import Compiler.Normalize (
  normalize
  )
import VM.VM (
  State (..),
  reduce,
  initializeHeap,
  showTransition,
  )
  
-- runtime
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

run :: Int -> State -> IO ()
run stepN oldState
  = case reduce oldState of
      Just (newState, rule)
        -> putStrLn
           (show stepN ++ ": "
             ++ showRule rule ++ " ~> \n"
             ++ show newState ++ "\n")
           >> run (stepN + 1) newState
      Nothing -> return ()

readAndRun:: String -> IO ()
readAndRun input = case normalize =<< compile input of
    Left err -> putStrLn ("Error : " ++ show err)
    Right (procVals, rules) ->
      let initialState = State (initializeHeap procVals) rules in
        putStrLn ("0: \n" ++ show initialState ++ "\n")
        >> run 1 initialState



{--|
main :: IO ()
main = do
  args <- getArgs
  putStrLn $ readExpr $ head args 
|--}
