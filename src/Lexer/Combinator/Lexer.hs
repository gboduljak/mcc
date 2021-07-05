{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lexer.Combinator.Lexer (lex') where

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
import Text.Megaparsec
  ( ErrorFancy (ErrorFail),
    ErrorItem (Label),
    MonadParsec (eof, lookAhead, notFollowedBy, observing, try, withRecovery),
    ParseError (FancyError, TrivialError),
    Parsec,
    anySingle,
    errorBundlePretty,
    errorOffset,
    getOffset,
    many,
    manyTill,
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
comment = singleLine <|> multiLine
  where
    singleLine :: Lexer ()
    singleLine = do
      string (pack "//")
      manyTill anySingle (void eol <|> lookAhead (try eof))
      return ()
    multiLine :: Lexer ()
    multiLine = do
      try $ string (pack "/*")
      manyTill (multiLine <|> void anySingle) (try $ string (pack "*/"))
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

literal :: Lexer Lexeme
literal = string' <|> char' <|> null <|> number <?> "literal"

string' :: Lexer Lexeme
string' =
  do
    char '"'
    x <- many (escapedChar <|> simpleChar)
    char '"'
    return (LitString x)

char' :: Lexer Lexeme
char' =
  do
    char '\''
    x <- escapedChar <|> simpleChar
    char '\''
    return (LitChar x)

escapedChar :: Lexer Char
escapedChar = do
  char '\\'
  x <- oneOf "\\\"abfnrt"
  case x of
    '\\' -> return x
    '"' -> return x
    'a' -> return '\a'
    'b' -> return '\b'
    'f' -> return '\f'
    'n' -> return '\n'
    'r' -> return '\r'
    't' -> return '\t'

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
    <|> identifier
    <|> literal
    <|> parens
    <|> operator

tokenWithRecovery :: Lexer Lexeme
tokenWithRecovery = do
  tok <- observing token
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
                    nonEmpty (show unexpected)
                )
            )
        )
        expected

tokens :: Lexer [Lexeme]
tokens = do
  junk
  toks <- sepEndBy tokenWithRecovery junk
  eof
  return toks

lex' :: String -> String
lex' x = case res of
  (Left p) -> errorBundlePretty p
  (Right tokens) -> show tokens
  where
    res = parse tokens "" (pack x)