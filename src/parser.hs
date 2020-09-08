module Parser (
  readExpr,
  showBlock,
  Pointer (Pointer, Atom),
  Proc (FromLocal, FromFreeTail, Rule),
  ParseError
  ) where
import Data.List
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Pointer = Pointer String
             | Atom String [Pointer]
             deriving(Eq, Ord)

data Proc = FromLocal (Maybe String) String [Pointer]
          | FromFreeTail String String [Pointer]
          | Alias String String
          | Rule (S.Set Proc) (S.Set Proc)
          deriving(Eq, Ord)

-- lexer
languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd    = "*/"
           , Token.commentLine    = "%"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum <|> char '_'
           , Token.reservedNames   = []
           , Token.reservedOpNames = []
           }                                     

lexer = Token.makeTokenParser languageDef

atomName :: Parser String
atomName =
  Token.lexeme lexer $ try $ 
  do c <- lower
     cs <- many (alphaNum <|> char '_')
     return $ c : cs

pointerName :: Parser String
pointerName = 
  Token.lexeme lexer $ try $
  do c <- (upper <|> char '_')
     cs <- many (alphaNum <|> char '_')
     return $ c : cs

parens      = Token.parens     lexer
reserevedOP = Token.reservedOp lexer
comma       = Token.comma      lexer
dot         = Token.dot        lexer 
whiteSpace  = Token.whiteSpace lexer 
arrow       = Token.lexeme lexer $ string "->"
turnstile   = Token.lexeme lexer $ string ":-"
star        = Token.lexeme lexer $ char '*'
commaSep    = Token.commaSep   lexer

-- parser
whileParser :: Parser (S.Set Proc)
whileParser = liftM (S.fromList) $ whiteSpace >> parseBlock
  
parseBlock :: Parser [Proc]
parseBlock = liftM concat $ sepEndBy1 parseLine dot
             
parseLine :: Parser [Proc]
parseLine = do x <- parseList
               (do y <- turnstile >> parseList
                   return [Rule (S.fromList x) (S.fromList y)]
                 ) <|> return x

parseList :: Parser [Proc]
parseList = liftM concat $ sepBy1 parseProc comma

parseAtomBody :: Parser (String, [Pointer])
parseAtomBody = do name <- atomName
                   args <- (parens $ commaSep parsePointer) <|> return []
                   return (name, args)

parsePointer :: Parser Pointer
parsePointer = (liftM Pointer pointerName)
               <|> return . uncurry Atom =<< parseAtomBody
  
parseProc :: Parser [Proc]
parseProc = (do (name, args) <- parseAtomBody
                return [FromLocal Nothing name args])
            <|> (do parent <- pointerName
                    (name, args) <- (arrow >> parseAtomBody)
                    return [FromLocal (Just parent) name args])
            <|> (do parent <- star >> pointerName
                    (name, args) <- (arrow >> parseAtomBody)
                    return [FromFreeTail parent name args])
            <|> parens parseLine

readExpr :: String -> Either ParseError (S.Set Proc)
readExpr = parse whileParser "vertex"

-- show
showBlock :: S.Set Proc -> String
showBlock = intercalate ". " . map showProc . S.toList

showProcList :: [Proc] -> String
showProcList = intercalate ", " . map showProc_
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
showProc (Rule lhs rhs) = showProcList (S.toList lhs) ++ " :- " ++ showProcList (S.toList rhs)
