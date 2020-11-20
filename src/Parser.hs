module Parser (
  readExpr,
  ParseError
  ) where
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Syntax 
import Data.Functor.Identity

-- lexer
languageDef :: GenLanguageDef String u Identity
languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "%"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum <|> char '_'
           , Token.reservedNames   = []
           , Token.reservedOpNames = []
           }                                     

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser languageDef


parens :: Parser a -> Parser a
parens      = Token.parens     lexer
-- reserevedOP = Token.reservedOp lexer
-- comma       = Token.comma      lexer

dot, turnstile, arrow :: Parser String
dot         = Token.dot        lexer 
turnstile   = Token.lexeme lexer $ string ":-"
arrow       = Token.lexeme lexer $ string "->"

whiteSpace :: Parser ()
whiteSpace  = Token.whiteSpace lexer 

backslash :: Parser Char
backslash   = Token.lexeme lexer $ char   '\\'

commaSep, commaSep1 :: Parser a -> Parser [a]
commaSep    = Token.commaSep   lexer 
commaSep1   = Token.commaSep1  lexer

atomName :: Parser String
atomName =
  Token.lexeme lexer $
  do c <- lower
     cs <- many (alphaNum <|> char '_')
     return $ c : cs
     
linkName :: Parser String
linkName = 
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
parseLine = do x <- parseList <|> return []
               ((do y <- (turnstile >> (parseList <|> return []))
                    return [RuleLit x y]
                ) <|> (return x))
               
parseList :: Parser [ProcLit]
parseList = liftM concat $ commaSep1 parseCreates

parseCreates :: Parser [ProcLit]
parseCreates = do creates <- endBy (backslash >> linkName) dot
                  process <- parseProc
                  return $ foldr (\x ps -> (:[]) $ CreationLit x ps) process creates 

parseAtomBody :: Parser (String, [LinkLit])
parseAtomBody = do name <- atomName
                   args <- (parens $ commaSep parseLink) <|> return []
                   return (name, args)

parseLink :: Parser LinkLit
parseLink = (liftM LinkLit linkName)
               <|> liftM (uncurry AtomLit)  parseAtomBody
  
parseProc :: Parser [ProcLit]
parseProc = (do from <- linkName
                to <- (arrow >> parseLink)
                return $ (:[]) $ AliasLit (Just from) to)
            <|> (liftM ((:[]) . AliasLit Nothing . uncurry AtomLit) parseAtomBody)
            <|> parens parseLine

readExpr :: String -> Either ParseError [ProcLit]
readExpr = parse whileParser "vertex"
