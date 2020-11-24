module Main where
import System.Environment
-- import Data.IORef
-- import Control.Monad.Except
-- import qualified Data.Map.Strict as M
-- import qualified Data.Set as S
-- import qualified Parser
-- import Syntax
import Compiler.Compiler (
  compile,
  showProcs
  )
import Compiler.Normalize (
  normalize
  )
import VM.Heap
import VM.VM
-- import Data.Tuple.Extra
import VM.FindAtom
import VM.PushAtom

{--|
readExpr :: String -> String
readExpr input = case Parser.readExpr input of
    Left err -> "No match : " ++ show err
    Right val -> "Parsed : " ++ showBlock val
      ++ "\n"
      ++ case compileProcs val of
           Left err -> "Compile Error : " ++ show err
           Right (addrSeed, procVals) ->
             "Compiled : \n\t addrSeed = " ++ show addrSeed
             ++ ", \n\t procVals = \n\t\t" ++ showProcVals procVals
|--}

readExpr :: String -> String
readExpr input = case normalize =<< compile input of
    Left err -> "Error : " ++ show err
    Right (procVals, rules) ->
      showProcs (procVals, rules) ++ "\n" ++
      show (findAtoms procVals initTestHeap)


main :: IO()
main = do
  args <- getArgs
  putStrLn $ show initTestHeap
  putStrLn $ readExpr $ head args 
