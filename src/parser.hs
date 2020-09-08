module Parser (
  readExpr,
  showBlock,
  Pointer (..),
  Proc (..),
  ParseError
  ) where
import Data.List
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
-- import qualified Data.Map.Strict as M
-- import qualified Data.Set as S

data Pointer = Pointer Int String
             | Atom String [Pointer]
             deriving(Eq, Ord)

data Proc = Alias (Maybe (Int, String)) Pointer
          | Rule [Proc] [Proc]
          | Molecule [Proc]
          deriving(Eq, Ord)

-- lexer
languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "%"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum <|> char '_'
           , Token.reservedNames   = []
           , Token.reservedOpNames = []
           }                                     

lexer = Token.makeTokenParser languageDef

parens      = Token.parens     lexer
reserevedOP = Token.reservedOp lexer
comma       = Token.comma      lexer
dot         = Token.dot        lexer 
whiteSpace  = Token.whiteSpace lexer 
commaSep    = Token.commaSep   lexer
arrow       = Token.lexeme lexer $ string "->"
turnstile   = Token.lexeme lexer $ string ":-"
star        = Token.lexeme lexer $ char   '*'
underscore  = Token.lexeme lexer $ char   '_'

atomName :: Parser String
atomName =
  Token.lexeme lexer $ try $ 
  do c <- lower
     cs <- many (alphaNum <|> char '_')
     return $ c : cs

pointerName :: Parser (Int, String)
pointerName = 
  Token.lexeme lexer $ try $
  do us <- many underscore 
     c <- upper
     cs <- many (alphaNum <|> char '_')
     return (length us, c : cs)

-- parser
whileParser :: Parser [Proc]
whileParser = whiteSpace >> parseBlock
  
parseBlock :: Parser [Proc]
parseBlock = sepEndBy1 parseLine dot
             
parseLine :: Parser Proc
parseLine = do x <- parseList
               ((do y <- (turnstile >> parseList)
                    return $ Rule x y
                ) <|> (return $ Molecule x))
               
parseList :: Parser [Proc]
parseList = sepBy1 parseProc comma

parseAtomBody :: Parser (String, [Pointer])
parseAtomBody = do name <- atomName
                   args <- (parens $ commaSep parsePointer) <|> return []
                   return (name, args)

parsePointer :: Parser Pointer
parsePointer = (liftM (uncurry Pointer) pointerName)
               <|> liftM (uncurry Atom)  parseAtomBody
  
parseProc :: Parser Proc
parseProc = (do from <- pointerName
                to <- (arrow >> parsePointer)
                return $ Alias (Just from) to)
            <|> (liftM (Alias Nothing . uncurry Atom) parseAtomBody)
            <|> parens parseLine

readExpr :: String -> Either ParseError [Proc]
readExpr = parse whileParser "vertex"

-- show
showBlock :: [Proc] -> String
showBlock = intercalate ". " . map showProc_
  where showProc_ (Molecule molecule) = showProcSet molecule
        showProc_ others              = showProc others

showProc :: Proc -> String
showProc (Alias (Just (i, p)) to) = showPointer (Pointer i p) ++ " -> " ++ showPointer to
showProc (Alias Nothing to) = showPointer to
showProc (Rule lhs rhs) = showProcSet lhs ++ " :- " ++ showProcSet rhs
showProc (Molecule molecule) = "(" ++ showProcSet molecule ++ ")"

showProcSet :: [Proc] -> String
showProcSet = intercalate ", " . map showProc_
  where showProc_ r@(Rule _ _) = "(" ++ showProc r ++ ")"
        showProc_ others = showProc others

showPointerList :: [Pointer] -> String
showPointerList [] = ""
showPointerList args = "(" ++ unwordsList args ++ ")"
  where unwordsList = intercalate ", " . map showPointer

showPointer :: Pointer -> String
showPointer (Pointer i name) = replicate i '_' ++ name
showPointer (Atom name args) = name ++ showPointerList args

