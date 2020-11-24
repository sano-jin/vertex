module Main where
import System.Environment
import Compiler.Compiler (
  compile,
  showProcs
  )
import Compiler.Normalize (
  normalize
  )
import VM.VM (
  State (..),
  run,
  initializeHeap,
  showTransition,
  )

readExpr :: String -> String
readExpr input = case normalize =<< compile input of
    Left err -> "Error : " ++ show err
    Right (procVals, rules) ->
      let initialHeap = initializeHeap procVals in
        showProcs (procVals, rules) ++
        "\n" ++ show initialHeap ++ "\n\n"
        ++ showTransition (run $ State initialHeap rules)


main :: IO ()
main = do
  args <- getArgs
  putStrLn $ readExpr $ head args 

