module Parser (
  readExpr,
  showBlock,
  Pointer (Pointer, Atom),
  Proc (FromLocal, FromFreeTail, Rule)
  ) where
import Data.List
import Text.ParserCombinators.Parsec

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c 

data Pointer = Pointer String
             | Atom String [Pointer]

data Proc = FromLocal (Maybe String) String [Pointer]
          | FromFreeTail String String [Pointer]
          | Alias String String
          | Rule [Proc] [Proc]

-- show
showBlock :: [Proc] -> String
showBlock = intercalate ". " . map showProc

showProcList :: [Proc] -> String
showProcList = intercalate ", " . map showProc_
  where showProc_ r@(Rule _ _) = "(" ++ showProc r ++ ")"
        showProc_ otherwise = showProc otherwise

showPointerList :: [Pointer] -> String
showPointerList [] = ""
showPointerList args = "(" ++ unwordsList args ++ ")"
  where unwordsList = intercalate ", " . map showPointer

showPointer :: Pointer -> String
showPointer (Pointer pointer) = pointer
showPointer (Atom name args) = name ++ showPointerList args

showProc :: Proc -> String
showProc (FromLocal (Just parent) name args) = parent ++ " -> " ++ name ++ showPointerList args
showProc (FromLocal Nothing name args) = name ++ showPointerList args
showProc (FromFreeTail parent name args) = "*" ++ parent ++ " -> " ++ name ++ showPointerList args
showProc (Rule lhs rhs) = showProcList lhs ++ " :- " ++ showProcList rhs

-- parser
pointerName :: Parser String
pointerName = do x <- upper <|> char '_'
                 y <- many (alphaNum <|> char '_')
                 return $ x : y 

atomName :: Parser String
atomName = (do x <- lower
               y <- many (alphaNum <|> char '_')
               return $ x : y)
           <|> do char '`'
                  x <- many $ noneOf "`"
                  char '`'
                  return x

parsePointer :: Parser Pointer
parsePointer = (return . Pointer =<< pointerName)
               <|> return . uncurry Atom =<< parseAtomBody

sepByComma :: Parser a -> Parser [a]
sepByComma parser = sepBy parser (spaces >> char ',' >> spaces)

paren :: Parser a -> Parser a
paren parser = do char '(' >> spaces
                  x <- parser
                  spaces >> char ')' >> spaces
                  return x

parenSepByComma :: Parser a -> Parser [a]
parenSepByComma parser = do char '(' >> spaces
                            x <- sepBy parser (spaces >> char ',' >> spaces)
                            spaces >> char ')' >> spaces
                            return x

parseAtomBody :: Parser (String, [Pointer])
parseAtomBody = do name <- atomName
                   spaces
                   args <- (parenSepByComma parsePointer)
                           <|> return []
                   return (name, args)
               
parseAtom :: Parser (String, String, [Pointer])
parseAtom = do parent <- pointerName
               spaces >> string "->" >> spaces
               (name, args) <- parseAtomBody
               return (parent, name, args)

parseProcess :: Parser [Proc]
parseProcess = (do char '*' >> spaces
                   parent <- pointerName
                   spaces >> string "->" >> spaces
                   (do (name, args) <- parseAtomBody
                       return [FromFreeTail parent name args])
                     <|> (do to <- pointerName
                             return $ [Alias parent to]))
               <|> (do (parent, name, args) <- parseAtom
                       return [FromLocal (Just parent) name args])
               <|> (return . (:[]) . uncurry (FromLocal Nothing) =<< parseAtomBody)
               <|> paren parseLine
                      
parseList :: Parser [Proc]
parseList = return . concat =<< sepByComma parseProcess  

parseLine :: Parser [Proc]
parseLine = do x <- parseList
               spaces
               (string ":-" >> spaces >> parseList >>= return . (:[]) . Rule x)
                 <|> return x

parseBlock :: Parser [Proc]
parseBlock = do spaces
                x <- sepBy parseLine $ try $ spaces >> char '.' >> space >> spaces
                (spaces >> char '.' >> spaces) <|> spaces
                return $ concat x

readExpr :: String -> Either ParseError [Proc]
readExpr = parse parseBlock "vertex"
