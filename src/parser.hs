module Main where
import System.Environment
import Text.ParserCombinators.Parsec

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

pointerName :: Parser String
pointerName = try upper >> many (alphaNum <|> char '_') >>= return 

atomName :: Parser String
atomName = do try lower >> many (alphaNum <|> char '_') >>= return
           <|> do char '`' 
                  x <- many $ noneOf "`"
                  char '`'
                  return x



readExpr :: String -> String
readExpr input = case parse pointerName "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value" 

data Pointer = Pointer String
             | UnAliasedAtom Atom
             
data Atom = Maybe String String [Pointer]
data Proc = Atom Atom
          | Rule [Atom] [Atom]

main :: IO()
main = do
  args <- getArgs
  putStrLn $ readExpr $ head args 
