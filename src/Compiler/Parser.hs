{-# LANGUAGE Safe #-}

module Compiler.Parser
  ( readExpr
  , ParseError
  ) where
import           Compiler.Syntax                ( LinkLit(..)
                                                , ProcLit(..)
                                                , Type(..)
                                                , DataAtom(..)
                                                )
import           Control.Applicative            ( liftA2 )
import           Data.Functor.Identity
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as Token

-- | Lexer
languageDef :: GenLanguageDef String u Identity
languageDef = emptyDef
  { Token.commentStart    = "/*"
  , Token.commentEnd      = "*/"
  , Token.commentLine     = "%"
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum <|> char '_' <|> char '\''
  , Token.opStart         = oneOf "+-*:=/><@|"
  , Token.opLetter        = oneOf "+-*:=/><@|"
  , Token.reservedNames   = []
  , Token.reservedOpNames = [ "+"
                            , "-"
                            , "*"
                            , "/"
                            , ":"
                            , ":="
                            , "="
                            , "/="
                            , "<="
                            , ">="
                            , "<"
                            , ">"
                            , "@@"
                            , "->"
                            , "\\"
                            , "|"
                            ]
  }

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser languageDef

parens :: Parser a -> Parser a
parens = Token.parens lexer

dot :: Parser String
dot = Token.dot lexer

integer :: Parser Integer
integer = Token.integer lexer

whiteSpace, colon, assign, backslash, dollar, atat, turnstile, arrow, bar
  :: Parser ()
whiteSpace = Token.whiteSpace lexer
colon     = reserved ":"
assign    = reserved ":="
backslash = reserved "\\"
dollar    = reserved "$"
atat      = reserved "@@"
turnstile = reserved ":-"
arrow     = reserved "->"
bar       = reserved "|"

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

reserved :: String -> Parser ()
reserved = Token.reservedOp lexer

atomName, linkName, stringLit :: Parser String
atomName =
  Token.lexeme lexer
    $   (liftA2 (:) lower $ many $ alphaNum <|> char '_' <|> char '\'')
    <|> (liftA2 (:) (char '\'') $ (++ "\'") <$> manyTill anyChar (char '\''))

linkName =
  Token.lexeme lexer
    $   liftA2 (:) (upper <|> char '_')
    $   many
    $   alphaNum
    <|> char '_'
    <|> char '\''

stringLit = Token.stringLiteral lexer

-- | Parser
whileParser, parseBlock, parseLine, parseList, parseCreates, parseProc
  :: Parser [ProcLit]
whileParser = (whiteSpace >> parseBlock) <* eof

parseBlock = concat <$> sepEndBy1 parseLine dot

parseLine = do
  maybeRuleName <- optionMaybe $ try $ atomName <* atat
  x             <- parseList
  (do
      y <- turnstile >> parseList
      ((: []) . RuleLit maybeRuleName x y <$> (bar >> parseList))
        <|> return [RuleLit maybeRuleName x [] y]
    )
    <|> case maybeRuleName of
          Just ruleName -> unexpected $ "rule name \"" ++ ruleName ++ " @@\""
          _             -> return x

parseList = concat <$> commaSep parseCreates

parseCreates = do
  creates <- endBy (backslash >> sepBy1 linkName whiteSpace) dot
  process <- parseProc
  return $ foldr (\x ps -> [CreationLit x ps]) process $ concat creates

parseAtom0, parseAtom4, parseAtom5, parseAtom7, parseAtom10, parseProcessContext, parseData
  :: Parser LinkLit
parseAtom0 = 
  try (do pCtx <- parseProcessContext 
          atom4 <- (assign >> parseAtom4)
          return $ AtomLit ":=" [pCtx, atom4]
      ) <|> parseAtom4

parseAtom4 = do
  left <- parseAtom5
  (do
      op <-
        "=" <$  reserved "="
        <|> "/=" <$  reserved "/="
        <|> ">"  <$  reserved ">"
        <|> "<"  <$  reserved "<"
        <|> "<=" <$  reserved "<="
        <|> ">=" <$  reserved ">="
      right <- parseAtom5
      return $ AtomLit op [left, right]
    )
    <|> return left

parseBinaryOp :: String -> Parser (LinkLit -> LinkLit -> LinkLit)
parseBinaryOp op = (\l r -> AtomLit op [l, r]) <$ reserved op

parseAtom5 = chainl1 parseAtom7 $ parseBinaryOp "+" <|> parseBinaryOp "-"

parseAtom7 = chainl1 parseAtom10 $ parseBinaryOp "*" <|> parseBinaryOp "/"

parseAtom10 =
  (liftA2 AtomLit atomName $ parens (commaSep parseAtom0) <|> return [])
    <|> parseProcessContext
    <|> parseData
    <|> LinkLit
    <$> linkName
    <|> parens parseAtom0

parseProcessContext = liftA2 ProcessContextLit (dollar >> atomName)
  $ optionMaybe (colon >> parseType)

parseType :: Parser Type
parseType =
  TypeInt
    <$  reserved "int"
    <|> TypeString
    <$  reserved "string"
    <|> TypeUnary
    <$  reserved "unary"

parseData = DataLit <$> (IntAtom <$> integer <|> StringAtom <$> stringLit)

parseProc =
  try (parens parseLine)
  <|> (try
       (do
           from <- linkName
           to   <- arrow >> parseAtom0
           return [AliasLit (Just from) to]
       )
      )
  <|> (do
          link <- parseAtom0
          case link of
            LinkLit linkNameStr ->
              unexpected $ "link \"" ++ linkNameStr ++ "\""
            _ -> return [AliasLit Nothing link]
      )
    
readExpr :: String -> Either ParseError [ProcLit]
readExpr = parse whileParser "vertex"
