module Main where
import System.Environment
import Data.IORef
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Parser (
  readExpr,
  showBlock,
  Pointer (Pointer, Atom),
  Proc (FromLocal, FromFreeTail, Rule),
  ParseError
  )

type Env = IORef (M.Map String (IORef (Int, Maybe Parser.Pointer)))

data CompileError = IsNotSerial String
                  | IsNotFunctional String
                  | RedundantAliasing String String
                  | Free2FreeAliasingOnLHS String String
                  | Parser Parser.ParseError

showError :: CompileError -> String
showError (IsNotSerial pointer) = "pointer '" ++ pointer ++ "' is not serial"
showError (IsNotFunctional pointer) = "pointer '" ++ pointer ++ "' is not functional"
showError (RedundantAliasing x y) = "aliasing from '" ++ x ++ "' to '" ++ y ++ "' is redundant"
showError (Free2FreeAliasingOnLHS x y) = "aliasing from free tail pointer '"
                                         ++ x ++ "' to free head pointer'" ++ y ++ "'"
instance Show CompileError where show = showError



nullEnv :: IO Env
nullEnv = newIORef M.empty
  
readExpr :: String -> String
readExpr input = case Parser.readExpr input of
    Left err -> "No match: " ++ show err
    Right val -> "Found " ++ Parser.showBlock val



main :: IO()
main = do
  args <- getArgs
  putStrLn $ readExpr $ head args 
