module Parser (
  readExpr,
  showBlock,
  Pointer (Pointer, Atom),
  Proc (FromLocal, FromFreeTail, Rule)
  ) where
import Data.List
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token as Token
import qualified Data.Map.Strict as M
import qualified Data.Set as S

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c 

data Pointer = Pointer String
             | Atom String [Pointer]

data Proc = FromLocal (Maybe String) String [Pointer]
          | FromFreeTail String String [Pointer]
          | Alias String String
          | Rule (S.Set Proc) (S.Set Proc)

languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentStart    = "*/"
           , Token.commentStart    = "%"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum <|> char '_'
           , Token.reservedNames   = []
           , Token.reservedOpNames = []
           }                                     

lexer = Token.makeTokenParser languageDef

atomName =
  Token.lexeme $ try $ do c <- lower
                          cs <- many (alphaNum <|> char '_')
                          return $ c : cs
pointerName = 
  Token.lexeme $ try $ do c <- (upper <|> '_')
                          cs <- many (alphaNum <|> char '_')
                          return $ c : cs

parens      = Token.parens     lexer
reserevedOP = Token.reservedOp lexer
comma       = Token.comma      lexer
dot         = Token.dot        lexer 
whiteSpace  = Token.whiteSpace lexer 
arrow       = Token.lexeme $ string "->"

-- parser
whileParser :: Parser (S.Set Proc)
whileParser = liftM (S.fromList) $ whiteSpace >> parseBlock
  
parseBlock :: Parser [Proc]
parseBlock = sepEndBy1 parseLine dot
             
parseLine :: Parser [Proc]
parseLine = do x <- parseList
               do char 
                    
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




-- show
showBlock :: S.Set Proc -> String
showBlock = intercalate ". " . map showProc . S.toList

showProcList :: S.Set Proc -> String
showProcList = intercalate ", " . map showProc_ . S.toList
  where showProc_ r@(Rule _ _) = "(" ++ showProc r ++ ")"
        showProc_ others = showProc others

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
