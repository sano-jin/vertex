{-# LANGUAGE Safe #-}

module Compiler.Parser
  ( readExpr
  , ParseError
  ) where
import           Compiler.Syntax                ( LinkLit(..)
                                                , ProcLit(..)
                                                )
import           Control.Applicative            ( liftA2 )
import           Data.Functor.Identity
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as Token

-- lexer
languageDef :: GenLanguageDef String u Identity
languageDef = emptyDef { Token.commentStart    = "/*"
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
parens = Token.parens lexer
-- reserevedOP = Token.reservedOp lexer
-- comma       = Token.comma      lexer

dot, turnstile, arrow :: Parser String
dot = Token.dot lexer
turnstile = Token.lexeme lexer $ string ":-"
arrow = Token.lexeme lexer $ string "->"

integer :: Parser Integer
integer = Token.integer lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

backslash :: Parser Char
backslash = Token.lexeme lexer $ char '\\'

commaSep, commaSep1 :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer
commaSep1 = Token.commaSep1 lexer

atomName :: Parser String
atomName = Token.lexeme lexer $ liftA2 (:) lower $ many $ alphaNum <|> char '_'

linkName :: Parser String
linkName = Token.lexeme lexer $ liftA2 (:) upper $ many $ alphaNum <|> char '_'

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

parseAtomBody :: Parser (String, [LinkLit])
parseAtomBody = liftA2 (,) atomName $ parens (commaSep parseLink) <|> return []

parseLink :: Parser LinkLit
parseLink =
  LinkLit
    <$> linkName
    <|> uncurry AtomLit
    <$> parseAtomBody
    <|> IntLit
    <$> integer

parseProc :: Parser [ProcLit]
parseProc =
  (do
      from <- linkName
      to   <- arrow >> parseLink
      return $ (: []) $ AliasLit (Just from) to
    )
    <|> (: [])
    .   AliasLit Nothing
    .   uncurry AtomLit
    <$> parseAtomBody
    <|> (: [])
    .   AliasLit Nothing
    .   IntLit
    <$> integer
    <|> parens parseLine

readExpr :: String -> Either ParseError [ProcLit]
readExpr = parse whileParser "vertex"
