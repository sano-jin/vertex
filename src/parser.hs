module Parser (
  readExpr,
  showBlock,
  ParseError,
  SourcePos
  ) where
import Data.List
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Syntax 

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
backslash   = Token.lexeme lexer $ char   '\\'

atomName :: Parser String
atomName =
  Token.lexeme lexer $
  do c <- lower
     cs <- many (alphaNum <|> char '_')
     return $ c : cs
     
pointerName :: Parser String
pointerName = 
  Token.lexeme lexer $
  do c <- upper
     cs <- many (alphaNum <|> char '_')
     return $ c : cs

-- parser
whileParser :: Parser [ProcLit]
whileParser = do x <- (whiteSpace >> parseBlock)
                 _ <- eof
                 return x
  
parseBlock :: Parser [ProcLit]
parseBlock = liftM concat $ sepEndBy1 parseLine dot
             
parseLine :: Parser [ProcLit]
parseLine = do x <- parseList
               ((do y <- (turnstile >> parseList)
                    return $ [RuleLit x y]
                ) <|> (return x))
               
parseList :: Parser [ProcLit]
parseList = liftM concat $ commaSep1 parseCreates

parseCreates :: Parser [ProcLit]
parseCreates = do creates <- endBy (backslash >> pointerName) dot
                  process <- parseProc
                  return $ foldr (\x ps -> (:[]) $ CreationLit x ps) process creates 

parseAtomBody :: Parser (String, [PointerLit])
parseAtomBody = do name <- atomName
                   args <- (parens $ commaSep parsePointer) <|> return []
                   return (name, args)

parsePointer :: Parser PointerLit
parsePointer = (liftM PointerLit pointerName)
               <|> liftM (uncurry AtomLit)  parseAtomBody
  
parseProc :: Parser [ProcLit]
parseProc = (do from <- pointerName
                to <- (arrow >> parsePointer)
                return $ (:[]) $ AliasLit (Just from) to)
            <|> (liftM ((:[]) . AliasLit Nothing . uncurry AtomLit) parseAtomBody)
            <|> parens parseLine

readExpr :: String -> Either ParseError [ProcLit]
readExpr = parse whileParser "vertex"

-- show
showBlock :: [ProcLit] -> String
showBlock = intercalate ". " . map showProc

showProc :: ProcLit -> String
showProc (AliasLit (Just p) to) =
  showPointer (PointerLit p) ++ " -> " ++ showPointer to
showProc (AliasLit Nothing to) = showPointer to
showProc (RuleLit lhs rhs) = showProcSet lhs ++ " :- " ++ showProcSet rhs
showProc (CreationLit pointer procs)
  = "\\" ++ pointer ++ "." ++ if length procs == 1 then showProcSet procs
                              else "(" ++ showProcSet procs ++ ")"                                
                                  
showProcSet :: [ProcLit] -> String
showProcSet = intercalate ", " . map showProc_
  where showProc_ r@(RuleLit _ _) = "(" ++ showProc r ++ ")"
        showProc_ others = showProc others

showPointerList :: [PointerLit] -> String
showPointerList [] = ""
showPointerList args = "(" ++ unwordsList args ++ ")"
  where unwordsList = intercalate ", " . map showPointer

showPointer :: PointerLit -> String
showPointer (PointerLit name) = name
showPointer (AtomLit name args) = name ++ showPointerList args

