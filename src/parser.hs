module Main where
import System.Environment
import Text.ParserCombinators.Parsec
import qualified Data.Map.Strict as M

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

parsePointer :: Parser Pointer
parsePointer = (return . flip Pointer 0 =<< pointerName)
               <|> return . uncurry Atom =<< parseAtom

sepByComma :: Parser a -> Parser [a]
sepByComma parser = sepBy parser (spaces >> char ',' >> spaces)

parseAtom :: Parser (String, [Pointer])
parseAtom = do name <- atomName
               args <- (do char '('
                           args <- sepByComma parsePointer
                           char ')'
                           return args)
                       <|> return []
               return (name, args)
               
alias :: Parser (String, String, [Pointer])
alias = do parent <- pointerName
           string "->"
           (name, args) <- parseAtom
           return (parent, name, args)

parseProcess :: Parser [Proc]
parseProcess = (do (parent, name, args) <- char '*' >> spaces >> alias
                   return [FromFreeTail (Pointer parent 0) name args])
               <|> (do (parent, name, args) <- alias
                       return [FromLocal (Just $ Pointer parent 0) name args])
               <|> (do (name, args) <- parseAtom
                       return [FromLocal Nothing name args])
               <|> do char '('
                      line <- parseLine
                      char ')'
                      return line
                      
parseLine :: Parser [Proc]
parseLine = return . concat =<< sepByComma parseProcess  

readExpr :: String -> String
readExpr input = case parse pointerName "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value" 

data Pointer = Pointer String Int
             | Atom String [Pointer]

data Proc = FromLocal (Maybe Pointer) String [Pointer]
          | FromFreeTail Pointer String [Pointer]
          | Rule [Proc] [Proc]
          | Molecule [Proc]

main :: IO()
main = do
  args <- getArgs
  putStrLn $ readExpr $ head args 
