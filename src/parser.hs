module Parser (
  readExpr,
  showBlock,
  PointerLit (..),
  ProcLit (..),
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

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

data PointerLit = PointerLit SourcePos Int String
             | AtomLit String [PointerLit]
             deriving(Eq, Ord, Show)

data ProcLit = AliasLit (Maybe (SourcePos, Int, String)) PointerLit
          | RuleLit [ProcLit] [ProcLit]
          | MoleculeLit [ProcLit]
          deriving(Eq, Ord, Show)

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
commaSep1   = Token.commaSep1  lexer
arrow       = Token.lexeme lexer $ string "->"
turnstile   = Token.lexeme lexer $ string ":-"
star        = Token.lexeme lexer $ char   '*'
underscore  = Token.lexeme lexer $ char   '_'

atomName :: Parser String
atomName =
  Token.lexeme lexer $
  do c <- lower
     cs <- many (alphaNum <|> char '_')
     return $ c : cs

pointerName :: Parser (SourcePos, Int, String)
pointerName = 
  Token.lexeme lexer $
  do pos <- getPosition
     us <- many underscore 
     c <- upper
     cs <- many (alphaNum <|> char '_')
     return (pos, length us, c : cs)

-- parser
whileParser :: Parser [ProcLit]
whileParser = do x <- (whiteSpace >> parseBlock)
                 _ <- eof
                 return x
  
parseBlock :: Parser [ProcLit]
parseBlock = sepEndBy1 parseLine dot
             
parseLine :: Parser ProcLit
parseLine = do x <- parseList
               ((do y <- (turnstile >> parseList)
                    return $ RuleLit x y
                ) <|> (return $ MoleculeLit x))
               
parseList :: Parser [ProcLit]
parseList = commaSep1 parseProc

parseAtomBody :: Parser (String, [PointerLit])
parseAtomBody = do name <- atomName
                   args <- (parens $ commaSep parsePointer) <|> return []
                   return (name, args)

parsePointer :: Parser PointerLit
parsePointer = (liftM (uncurry3 PointerLit) pointerName)
               <|> liftM (uncurry AtomLit)  parseAtomBody
  
parseProc :: Parser ProcLit
parseProc = (do from <- pointerName
                to <- (arrow >> parsePointer)
                return $ AliasLit (Just from) to)
            <|> (liftM (AliasLit Nothing . uncurry AtomLit) parseAtomBody)
            <|> parens parseLine

readExpr :: String -> Either ParseError [ProcLit]
readExpr = parse whileParser "vertex"

-- show
showBlock :: [ProcLit] -> String
showBlock = intercalate ". " . map showProc_
  where showProc_ (MoleculeLit molecule) = showProcSet molecule
        showProc_ others              = showProc others

showProc :: ProcLit -> String
showProc (AliasLit (Just (pos, i, p)) to) =
  showPointer (PointerLit pos i p) ++ " -> " ++ showPointer to
showProc (AliasLit Nothing to) = showPointer to
showProc (RuleLit lhs rhs) = showProcSet lhs ++ " :- " ++ showProcSet rhs
showProc (MoleculeLit molecule) = "(" ++ showProcSet molecule ++ ")"

showProcSet :: [ProcLit] -> String
showProcSet = intercalate ", " . map showProc_
  where showProc_ r@(RuleLit _ _) = "(" ++ showProc r ++ ")"
        showProc_ others = showProc others

showPointerList :: [PointerLit] -> String
showPointerList [] = ""
showPointerList args = "(" ++ unwordsList args ++ ")"
  where unwordsList = intercalate ", " . map showPointer

showPointer :: PointerLit -> String
showPointer (PointerLit _ i name) = replicate i '_' ++ name
showPointer (AtomLit name args) = name ++ showPointerList args

