module Main where
import System.Environment
import Data.IORef
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Parser (
  readExpr,
  showBlock,
  Pointer (..),
  Proc (..),
  ParseError
  )

type Env = IORef (M.Map String (IORef (Int, Maybe Parser.Pointer)))

data CompileError = IsNotSerial String
                  | IsNotFunctional String
                  | RedundantAliasing String String
                  | Free2FreeAliasingOnLHS String String
                  | Parser Parser.ParseError

-- show
showError :: CompileError -> String
showError (IsNotSerial pointer) = "pointer '" ++ pointer ++ "' is not serial"
showError (IsNotFunctional pointer) = "pointer '" ++ pointer ++ "' is not functional"
showError (RedundantAliasing x y) = "aliasing from '" ++ x ++ "' to '" ++ y ++ "' is redundant"
showError (Free2FreeAliasingOnLHS x y) = "aliasing from free tail pointer '"
                                         ++ x ++ "' to free head pointer'" ++ y ++ "'"
showError (Parser parseError) = "Parse error at " ++ show parseError
instance Show CompileError where show = showError

type ThrowsError = Either CompileError
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

nullEnv :: IO Env
nullEnv = newIORef M.empty
  
readExpr :: String -> String
readExpr input = case Parser.readExpr input of
    Left err -> "No match : " ++ show err
    Right val -> "Found : " ++ Parser.showBlock val



main :: IO()
main = do
  args <- getArgs
  putStrLn $ readExpr $ head args 
