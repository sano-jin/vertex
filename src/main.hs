module Main where
import System.Environment
import Data.IORef
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Parser (
  readExpr,
  showBlock,
  Pointer (Pointer, Atom),
  Proc (FromLocal, FromFreeTail, Rule)
  )

type Env = IORef (M.Map String (IORef (Int, Maybe Parser.Pointer)))

data CanonicError = IsNotSerial String
                  | IsNotFunctional String

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
