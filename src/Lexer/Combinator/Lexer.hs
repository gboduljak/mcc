module Lexer.Combinator.Lexer (lex', lexIncludes, lexIncludes') where

import Control.Applicative (Alternative ((<|>)), optional)
import Control.Monad (void)
import Data.Char (isDigit, isSpace)
import Data.Either (fromRight)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (Maybe (Just, Nothing), fromJust)
import Data.Text (Text, pack)
import Data.Void (Void)
import Lexer.Lexeme
  ( BuiltinType (Char, Double, Int, Void),
    Lexeme (..),
  )
import Lexer.Token (Token (Token))
import Text.Megaparsec
  ( ErrorFancy (ErrorFail),
    ErrorItem (Label),
    MonadParsec (eof, lookAhead, notFollowedBy, observing, try, withRecovery),
    ParseError (FancyError, TrivialError),
    ParseErrorBundle (ParseErrorBundle),
    Parsec,
    SourcePos (sourceColumn),
    anySingle,
    anySingleBut,
    errorBundlePretty,
    errorOffset,
    getOffset,
    getSourcePos,
    many,
    manyTill,
    mkPos,
    noneOf,
    oneOf,
    parse,
    parseErrorPretty,
    parseErrorTextPretty,
    parseTest,
    registerParseError,
    runParser,
    satisfy,
    sepBy,
    sepEndBy,
    single,
    skipMany,
    skipManyTill,
    skipSomeTill,
    some,
    unPos,
    unexpected,
    (<?>),
  )
import Text.Megaparsec.Byte (string)
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, eol, letterChar)
import Text.Megaparsec.Error (ErrorItem (Tokens), showErrorComponent)
import Prelude hiding (and, div, mod, not, null, or)

type Lexer = Parsec Void Text

space :: Lexer ()
space = void $ satisfy isSpace

comment :: Lexer ()
comment = try singleLine <|> try multiLine
  where
    singleLine :: Lexer ()
    singleLine = do
      string (pack "//")
      manyTill anySingle (void eol <|> lookAhead (try eof))
      return ()
    multiLine :: Lexer ()
    multiLine = do
      string (pack "/*")
      manyTill anySingle (try $ string (pack "*/"))
      return ()

junk :: Lexer ()
junk = skipMany (space <|> comment)

reserved :: String -> Lexer ()
reserved word = try (string (pack word) *> notFollowedBy alphaNumChar)

symbol :: String -> Lexer Text
symbol sym = string (pack sym)

lparen :: Lexer Lexeme
lparen = symbol "(" >> return LParen

rparen :: Lexer Lexeme
rparen = symbol ")" >> return RParen

lbrace :: Lexer Lexeme
lbrace = symbol "{" >> return LBrace

rbrace :: Lexer Lexeme
rbrace = symbol "}" >> return RBrace

lbrack :: Lexer Lexeme
lbrack = symbol "[" >> return LBrack

rbrack :: Lexer Lexeme
rbrack = symbol "]" >> return RBrack

plus :: Lexer Lexeme
plus = symbol "+" >> return Plus

minus :: Lexer Lexeme
minus = symbol "-" >> return Minus

asterisk :: Lexer Lexeme
asterisk = symbol "*" >> return Asterisk

div :: Lexer Lexeme
div = symbol "/" >> return Div

mod :: Lexer Lexeme
mod = symbol "%" >> return Mod

equal :: Lexer Lexeme
equal = symbol "==" >> return Equal

assign :: Lexer Lexeme
assign = symbol "=" >> return Assign

notEqual :: Lexer Lexeme
notEqual = symbol "!=" >> return Neq

less :: Lexer Lexeme
less = symbol "<" >> return Less

leq :: Lexer Lexeme
leq = symbol "<=" >> return Leq

greater :: Lexer Lexeme
greater = symbol ">" >> return Greater

geq :: Lexer Lexeme
geq = symbol ">=" >> return Geq

and :: Lexer Lexeme
and = symbol "&&" >> return And

or :: Lexer Lexeme
or = symbol "||" >> return Or

not :: Lexer Lexeme
not = symbol "!" >> return Not

ampers :: Lexer Lexeme
ampers = symbol "&" >> return Ampers

bar :: Lexer Lexeme
bar = symbol "|" >> return Bar

caret :: Lexer Lexeme
caret = symbol "^" >> return Caret

dot :: Lexer Lexeme
dot = symbol "." >> return Dot

arrow :: Lexer Lexeme
arrow = symbol "->" >> return Arrow

comma :: Lexer Lexeme
comma = symbol "," >> return Comma

semi :: Lexer Lexeme
semi = symbol ";" >> return Semi

for :: Lexer Lexeme
for = reserved "for" >> return For

while :: Lexer Lexeme
while = reserved "while" >> return While

if' :: Lexer Lexeme
if' = reserved "if" >> return If

else' :: Lexer Lexeme
else' = reserved "else" >> return Else

struct :: Lexer Lexeme
struct = reserved "struct" >> return Struct

return' :: Lexer Lexeme
return' = reserved "return" >> return Return

sizeof :: Lexer Lexeme
sizeof = reserved "sizeof" >> return Sizeof

null :: Lexer Lexeme
null = reserved "NULL" >> return LitNull

include :: Lexer Lexeme
include = reserved "#include" >> return Include

literal :: Lexer Lexeme
literal = litString <|> litChar <|> null <|> number <?> "literal"

string' :: Lexer String
string' = do
  char '"'
  manyTill (escapedChar <|> noneOf "\n\r") (char '"')

litString :: Lexer Lexeme
litString = LitString <$> string'

litChar :: Lexer Lexeme
litChar =
  do
    char '\''
    x <- escapedChar <|> simpleChar
    char '\''
    return (LitChar x)

escapedChar :: Lexer Char
escapedChar = do
  char '\\'
  x <- oneOf "\\\"abfnrt0"
  case x of
    '\\' -> return x
    '"' -> return x
    'a' -> return '\a'
    'b' -> return '\b'
    'f' -> return '\f'
    'n' -> return '\n'
    'r' -> return '\r'
    't' -> return '\t'
    '0' -> return '\0'
    x -> return x

simpleChar :: Lexer Char
simpleChar = noneOf "\""

number :: Lexer Lexeme
number =
  do
    intPt <- digits
    maybeDot <- optional (char '.')
    case maybeDot of
      (Just dot) -> do
        fracPt <- digits
        return (LitDouble (read (intPt ++ "." ++ fracPt)))
      Nothing -> return (LitInt (read intPt)) <?> "number"
  where
    digits :: Lexer String
    digits = some digitChar

typeName :: Lexer Lexeme
typeName =
  (reserved "int" >> return (Type Int))
    <|> (reserved "double" >> return (Type Double))
    <|> (reserved "char" >> return (Type Char))
    <|> (reserved "void" >> return (Type Void)) <?> "type"

identifier :: Lexer Lexeme
identifier = do
  x <- letterChar <|> char '_'
  xs <- many (alphaNumChar <|> char '_')
  return (Ident (x : xs)) <?> "identifier"

includeStmt :: Lexer String
includeStmt = do
  include >> skipMany space >> string'

includes :: Lexer [String]
includes = junk >> sepEndBy includeStmt junk

directive :: Lexer Lexeme
directive = include

operator :: Lexer Lexeme
operator =
  try arrow
    <|> try and
    <|> try or
    <|> try leq
    <|> try geq
    <|> try equal
    <|> try notEqual
    <|> semi
    <|> assign
    <|> comma
    <|> dot
    <|> ampers
    <|> not
    <|> minus
    <|> plus
    <|> asterisk
    <|> div
    <|> mod
    <|> less
    <|> greater
    <|> caret
    <|> bar <?> "operator"

keyword :: Lexer Lexeme
keyword =
  for
    <|> while
    <|> if'
    <|> else'
    <|> struct
    <|> sizeof
    <|> return' <?> "keyword"

parens :: Lexer Lexeme
parens =
  lparen
    <|> rparen
    <|> lbrace
    <|> rbrace
    <|> lbrack
    <|> rbrack

token :: Lexer Lexeme
token =
  keyword
    <|> typeName
    <|> literal
    <|> identifier
    <|> parens
    <|> operator
    <|> directive

withPos :: Lexer Lexeme -> Lexer Token
withPos lexer = do
  start <- getSourcePos
  x <- lexer
  end <- getSourcePos

  let length = (unPos . sourceColumn) end - (unPos . sourceColumn) start
  return (Token x start end length)

tokenWithRecovery :: Lexer Lexeme
tokenWithRecovery = do
  tok <- observing token
  junk
  case tok of
    (Left (TrivialError _ _ expected)) -> do
      offset <- getOffset
      unexpectedChar <- anySingle
      registerParseError (buildError offset unexpectedChar expected)
      junk
      return Error
    (Left error) -> do
      registerParseError error
      junk
      return Error
    (Right tok') -> return tok'
  where
    buildError offset unexpected expected =
      TrivialError
        offset
        ( Just
            ( Label
                ( fromJust $
                    nonEmpty [unexpected]
                )
            )
        )
        expected

tokens :: Lexer [Token]
tokens = do
  junk
  toks <- many (withPos tokenWithRecovery)
  eof <- withPos lexEof
  return (toks ++ [eof])
  where
    lexEof :: Lexer Lexeme
    lexEof = eof >> return Eof

lexIncludes :: String -> Text -> Either (ParseErrorBundle Text Void) [String]
lexIncludes = parse (junk >> includes)

lexIncludes' :: String -> String -> Either (ParseErrorBundle Text Void) [String]
lexIncludes' file input = parse (junk >> includes) file (pack input)

lex :: String -> Text -> Either (ParseErrorBundle Text Void) [Token]
lex = parse tokens

lex' :: String -> String -> Either (ParseErrorBundle Text Void) [Token]
lex' file input = parse tokens file (pack input)