{-# LANGUAGE Safe #-}

module Compiler.Parser
  ( readExpr
  , ParseError
  ) where
import           Compiler.Syntax                ( LinkLit  (..)
                                                , ProcLit  (..)
                                                , Type     (..)
                                                , DataAtom (..)
                                                )
import           Control.Applicative            ( liftA2 )
import           Data.Functor.Identity
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as Token

-- lexer
languageDef :: GenLanguageDef String u Identity
languageDef
  = emptyDef { Token.commentStart    = "/*"
             , Token.commentEnd      = "*/"
             , Token.commentLine     = "%"
             , Token.identStart      = letter
             , Token.identLetter     = alphaNum <|> char '_' <|> char '\''
             , Token.opStart         = oneOf "+-*:=/><"
             , Token.opLetter        = oneOf "+-*:=/><"
             , Token.reservedNames   = []
             , Token.reservedOpNames =
                 ["+", ":", ":=", "*", "=", "/=", "<=", ">=", "<", ">"]
             }

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser languageDef


parens :: Parser a -> Parser a
parens = Token.parens lexer
-- reserevedOP = Token.reservedOp lexer
-- comma       = Token.comma      lexer

dot, turnstile, arrow :: Parser String
dot       = Token.dot lexer
turnstile = Token.lexeme lexer $ string ":-"
arrow     = Token.lexeme lexer $ string "->"

integer :: Parser Integer
integer = Token.integer lexer

whiteSpace, colon, assign :: Parser ()
whiteSpace = Token.whiteSpace lexer
colon      = reserved ":"
assign     = reserved ":="

backslash, dollar :: Parser Char
backslash = Token.lexeme lexer $ char '\\'
dollar    = Token.lexeme lexer $ char '$'

commaSep, commaSep1 :: Parser a -> Parser [a]
commaSep  = Token.commaSep lexer
commaSep1 = Token.commaSep1 lexer

reserved :: String -> Parser ()
reserved = Token.reservedOp lexer

atomName :: Parser String
atomName = Token.lexeme lexer $ liftA2 (:) lower $ many $ alphaNum <|> char '_' <|> char '\''

linkName :: Parser String
linkName = Token.lexeme lexer $ liftA2 (:) upper $ many $ alphaNum <|> char '_' <|> char '\''

stringLit :: Parser String
stringLit = Token.stringLiteral lexer

-- parser
whileParser :: Parser [ProcLit]
whileParser = (whiteSpace >> parseBlock) <* eof

parseBlock :: Parser [ProcLit]
parseBlock = concat <$> sepEndBy1 parseLine dot

parseLine :: Parser [ProcLit]
parseLine = do
  x <- parseList <|> return []
  (do
      y <- turnstile >> (parseList <|> return [])
      return [RuleLit x y]
    )
    <|> return x

parseList :: Parser [ProcLit]
parseList = concat <$> commaSep1 parseCreates

parseCreates :: Parser [ProcLit]
parseCreates = do
  creates <- endBy (backslash >> sepBy1 linkName whiteSpace) dot
  process <- parseProc
  return $ foldr (\x ps -> [CreationLit x ps]) process $ concat creates

parseAtom10 :: Parser LinkLit
parseAtom10 = (liftA2 AtomLit atomName $ parens (commaSep parseLink) <|> return [])
              <|> parseProcessContext
              <|> parseData
              <|> LinkLit
              <$> linkName
              <|> parens parseAtom0

parseAtom0 :: Parser LinkLit
parseAtom0 =
  (do pCtx <- parseProcessContext 
      (do atom4 <- (assign >> parseAtom4)
          return $ AtomLit ":=" [pCtx, atom4])
        <|> return pCtx)
  <|> parseAtom4

parseAtom4 :: Parser LinkLit
parseAtom4 = do
  left <- parseAtom5
  (do op <- "=" <$ reserved "="
            <|> "/=" <$ reserved "/="
            <|> ">"  <$ reserved ">" 
            <|> "<"  <$ reserved "<" 
            <|> "<=" <$ reserved "<="
            <|> ">=" <$ reserved ">="
      right <- parseAtom5
      return $ AtomLit op [left, right])
    <|> return left

parseAtom5 :: Parser LinkLit
parseAtom5 = chainl1 parseAtom7
             $ (\l r -> AtomLit "+" [l, r]) <$ reserved "+"
             <|> (\l r -> AtomLit "-" [l, r]) <$ reserved "-"
parseAtom7 :: Parser LinkLit
parseAtom7 = chainl1 parseAtom10
             $ (\l r -> AtomLit "*" [l, r]) <$ reserved "*"
             <|> (\l r -> AtomLit "/" [l, r]) <$ reserved "/"
       
parseProcessContext :: Parser LinkLit
parseProcessContext
  = liftA2 ProcessContextLit (dollar >> atomName) $ optionMaybe (colon >> parseType) 

parseType :: Parser Type
parseType
  = TypeInt        <$ reserved "int"
    <|> TypeString <$ reserved "string"
    <|> TypeUnary  <$ reserved "unary"

parseData :: Parser LinkLit
parseData
  = DataLit <$> (IntAtom <$> integer
                 <|> StringAtom <$> stringLit)
      
parseLink :: Parser LinkLit
parseLink = parseAtom0

parseProc :: Parser [ProcLit]
parseProc =
  (try (do from <- linkName
           to   <- arrow >> parseLink
           return [AliasLit (Just from) to])
  )
  <|> (do link <- parseAtom0
          case link of
            LinkLit linkName
              -> unexpected $ "link \"" ++ linkName ++ "\""
            _ -> return [AliasLit Nothing link])
  <|> parens parseLine

readExpr :: String -> Either ParseError [ProcLit]
readExpr = parse whileParser "vertex"
