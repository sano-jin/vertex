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
import           Control.Monad                  ( ap )
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
  , Token.opStart         = oneOf "+-*:=/><@|\\"
  , Token.opLetter        = oneOf "+-*:=/><@|\\"
  , Token.reservedNames   = []
  , Token.reservedOpNames = [ "+", "-", "*", "/"
                            , "=", "/=", "<=", ">=", "<", ">"
                            , ":", ":-", ":=", "@@", "->", "\\", "|"
                            ]
  }

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser languageDef

parens, brackets :: Parser a -> Parser a
parens = Token.parens lexer
brackets = Token.brackets lexer

integer :: Parser Integer
integer = Token.integer lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

reservedOp :: String -> Parser String
reservedOp op = op <$ Token.reservedOp lexer op

colon, assign, backslash, dollar, atat, turnstile, arrow, bar, dot :: Parser String
colon     = reservedOp ":"
assign    = reservedOp ":="
backslash = reservedOp "\\"
dollar    = reservedOp "$"
atat      = reservedOp "@@"
turnstile = reservedOp ":-"
arrow     = reservedOp "->"
bar       = reservedOp "|"
dot       = Token.dot lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

atomName, linkName, stringLit :: Parser String
atomName =
  Token.lexeme lexer
    $   liftA2 (:) lower       (many $ alphaNum <|> char '_' <|> char '\'')
    <|> liftA2 (:) (char '\'') ((++ "'") <$> manyTill anyChar (char '\''))

linkName =
  Token.lexeme lexer
    $ liftA2 (:) (upper <|> char '_')
    $ many $ alphaNum <|> char '_' <|> char '\''

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
  try (liftA2 (binaryAtom ":=") parseProcessContext $ assign >> parseAtom4)
    <|> parseAtom4

parseAtom4 = do
  left <- parseAtom5
  parseBinaryOps ["=", "/=", ">", "<", "<=", ">="] `ap` return left `ap` parseAtom5
    <|> return left

binaryAtom :: String -> LinkLit -> LinkLit -> LinkLit
binaryAtom name l r = AtomLit ("'" ++ name ++ "'") [l, r]

parseBinaryOps :: [String] -> Parser (LinkLit -> LinkLit -> LinkLit)
parseBinaryOps = choice . map (fmap binaryAtom . reservedOp)

parseAtom5 = chainl1 parseAtom7  $ parseBinaryOps ["+", "-"]
parseAtom7 = chainl1 parseAtom10 $ parseBinaryOps ["*", "/"]

parseAtom10 =
  liftA2 AtomLit atomName (parens (commaSep parseAtom0) <|> return [])
    <|> parseProcessContext
    <|> parseData
    <|> LinkLit <$> linkName
    <|> parens parseAtom0
    <|> parseListAbbreviation

parseProcessContext =
  liftA2 ProcessContextLit (dollar >> atomName)
  $ optionMaybe $ colon >> parseType

parseType :: Parser Type
parseType = TypeInt    <$  reservedOp "int"
        <|> TypeString <$  reservedOp "string"
        <|> TypeUnary  <$  reservedOp "unary"

parseData = DataLit <$> (IntAtom <$> integer <|> StringAtom <$> stringLit)

parseProc =
  parens parseLine
    <|> try
          (do
            from <- linkName
            to   <- arrow >> parseAtom0
            return [AliasLit (Just from) to]
          )
    <|> (checkNotLink =<< parseAtom0)
 where
  checkNotLink (LinkLit linkNameStr) =
    unexpected $ "link \"" ++ linkNameStr ++ "\""
  checkNotLink link = return [AliasLit Nothing link]

parseListAbbreviation :: Parser LinkLit
parseListAbbreviation = brackets $ do
  leftElems <- commaSep parseAtom0
  flip (foldr $ binaryAtom ".") leftElems
    <$> ((bar >> parseAtom0) <|> return (AtomLit "[]" []))

readExpr :: String -> Either ParseError [ProcLit]
readExpr = parse whileParser "vertex"
