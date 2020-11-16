module Main where
import System.Environment
import Data.IORef
import Control.Monad.Except
-- import qualified Data.Map.Strict as M
-- import qualified Data.Set as S
import qualified Parser (
  readExpr,
  ParseError,
  SourcePos
  )
import Syntax

readExpr :: String -> String
readExpr input = case Parser.readExpr input of
    Left err -> "No match : " ++ show err
    Right val -> "Found : " ++ showBlock val



main :: IO()
main = do
  args <- getArgs
  putStrLn $ readExpr $ head args 
