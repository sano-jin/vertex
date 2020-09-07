module Main where
import System.Environment
import qualified Data.Map.Strict (..) as M
import qualified Parser (
  readExpr,
  showBlock,
  Pointer (Pointer, Atom),
  Proc (FromLocal, FromFreeTail, Rule)
  )
  
readExpr :: String -> String
readExpr input = case Parser.readExpr input of
    Left err -> "No match: " ++ show err
    Right val -> "Found " ++ Parser.showBlock val



main :: IO()
main = do
  args <- getArgs
  putStrLn $ readExpr $ head args 
