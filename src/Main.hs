module Main where
import System.Environment
import Data.IORef
import Control.Monad.Except
-- import qualified Data.Map.Strict as M
-- import qualified Data.Set as S
import qualified Parser
import Syntax
import Compiler

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
readExpr input = case compile input of
    Left err -> "Error : " ++ show err
    Right procs -> showProcs procs

main :: IO()
main = do
  args <- getArgs
  putStrLn $ readExpr $ head args 
