{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Parser.Combinator.Parser where

import qualified Data.List as DL
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Set as Set
import Data.Void
import Lexer.Combinator.Lexer (lex')
import Lexer.Lexeme as L
import Lexer.Token as T
import qualified Parser.Ast as Ast
import Parser.AstVisualiser (visualiseAst)
import Parser.Combinator.Chains
import Parser.Combinator.TokenStream
import Text.Megaparsec
import Prelude hiding (negate)

type Parser = Parsec Void TokenStream

liftToken :: L.Lexeme -> T.Token
liftToken lexeme = T.Token lexeme pos pos 0
  where
    pos = initialPos ""

expect :: L.Lexeme -> Parser L.Lexeme
expect lex = token test (Set.singleton . Tokens . nes . liftToken $ lex)
  where
    test (Token lex' _ _ _) =
      if lex' == lex
        then Just lex
        else Nothing
    nes x = x :| []

program :: Parser Ast.Program
program = do
  dirs <- directives
  Ast.Program dirs <$> constructs

directives :: Parser [Ast.Directive]
directives = includes

constructs :: Parser [Ast.Construct]
constructs = many construct

construct :: Parser Ast.Construct
construct = do
  constr <- observing (structDecl <|> funcOrVarDecl)
  case constr of
    (Right decl) -> return decl
    (Left error) -> do
      registerParseError error
      takeWhile1P (Just "") (not . followsConstruct . lexeme)
      return (Ast.ConstructError)
  where
    followsConstruct L.Struct = True
    followsConstruct (L.Type _) = True
    followsConstruct (L.Eof) = True
    followsConstruct _ = False

structDecl :: Parser Ast.Construct
structDecl = do
  expect L.Struct
  nameId <- ident
  vars <- between (expect L.LBrace) (expect L.RBrace) (many field)
  expect Semi
  return (Ast.StructDecl $ Ast.Struct (name nameId) vars)
  where
    field = do
      typ <- type'
      nameId <- ident
      varDecl typ (name nameId)
    name (Ast.Ident x) = x

funcOrVarDecl :: Parser Ast.Construct
funcOrVarDecl = do
  typ <- type'
  nameId <- ident
  funcDecl' typ nameId <|> varDecl' typ nameId
  where
    funcDecl' typ nameId = Ast.FuncDecl <$> funcDecl typ (name nameId)
    varDecl' typ nameId = Ast.VarDecl <$> varDecl typ (name nameId)
    name (Ast.Ident x) = x

funcDecl :: Ast.Type -> String -> Parser Ast.FuncDecl
funcDecl rettyp funcName = do
  args <- between (expect L.LParen) (expect L.RParen) formals
  Ast.Func rettyp funcName args <$> block

varDecl :: Ast.Type -> String -> Parser Ast.VarDecl
varDecl typ name = do
  sizes <- arraySizes
  expect Semi
  return (Ast.Var typ name sizes)

statement :: Parser Ast.Statement
statement = do
  stmt <-
    observing
      ( exprStmt <|> varDeclStmt
          <|> blockStmt
          <|> returnStmt
          <|> while
          <|> for
          <|> if'
          <|> varDeclStmt
      )
  case stmt of
    (Right s) -> return s
    (Left error) -> do
      registerParseError error
      recoverUsingFollow
  where
    exprStmt = do exp <- expr; expect Semi; return (Ast.Expr exp)
    blockStmt = do Ast.BlockStatement <$> block
    returnStmt = do ret <- return'; expect Semi; return ret
    varDeclStmt = do
      typ <- type'
      nameId <- ident
      Ast.VarDeclStatement <$> varDecl typ (varName nameId)
      where
        varName (Ast.Ident name) = name
    recoverUsingFollow = do
      takeWhile1P (Just "") (not . followsStatement . lexeme)
      return Ast.StatementError

    followsStatement L.Else = True
    followsStatement L.While = True
    followsStatement L.For = True
    followsStatement L.If = True
    followsStatement L.Return = True
    followsStatement L.LBrace = True
    followsStatement L.Struct = True
    followsStatement (L.Type _) = True
    followsStatement L.LParen = True
    followsStatement L.Minus = True
    followsStatement L.Not = True
    followsStatement L.Asterisk = True
    followsStatement L.Ampers = True
    followsStatement L.Sizeof = True
    followsStatement (L.LitInt _) = True
    followsStatement (L.LitDouble _) = True
    followsStatement (L.LitChar _) = True
    followsStatement (L.LitString _) = True
    followsStatement (L.LitNull) = True
    followsStatement (L.Ident _) = True
    followsStatement (L.RBrace) = True
    followsStatement _ = False

-- statement :: Parser Ast.Statement
-- statement =
--   exprStmt
--     <|> blockStmt
--     <|> returnStmt
--     <|> while
--     <|> for
--     <|> if'
--     <|> varDeclStmt
--   where
--     exprStmt = do exp <- expr; expect Semi; return (Ast.Expr exp)
--     blockStmt = do Ast.BlockStatement <$> block
--     returnStmt = do ret <- return'; expect Semi; return ret
--     varDeclStmt = do
--       typ <- type'
--       nameId <- ident
--       Ast.VarDeclStatement <$> varDecl typ (varName nameId)
--       where
--         varName (Ast.Ident name) = name
--     followStatement = [Semi]

while :: Parser Ast.Statement
while = do
  expect L.While
  cond <- between (expect L.LParen) (expect L.RParen) recoverExp
  Ast.While cond <$> statement

for :: Parser Ast.Statement
for = do
  expect L.For
  expect L.LParen
  init <- optional recoverExp
  expect L.Semi
  cond <- optional recoverExp
  expect L.Semi
  incr <- optional recoverExp
  expect L.RParen
  Ast.For init cond incr <$> statement

if' :: Parser Ast.Statement
if' = do
  expect L.If
  cond <- between (expect L.LParen) (expect L.RParen) recoverExp
  conseq <- statement
  alt <- optional (expect L.Else >> statement)
  return (Ast.If cond conseq alt)

return' :: Parser Ast.Statement
return' = do
  expect L.Return
  Ast.Return <$> optional expr

block :: Parser Ast.Block
block = do
  expect L.LBrace
  stmts <- manyTill statement (expect L.RBrace)

  return (Ast.Block stmts)

recoverExp :: Parser Ast.Expr
recoverExp = do
  exp <- observing assignLevelExpr
  case exp of
    (Right exp) -> return exp
    (Left error) -> do
      registerParseError error
      takeWhile1P (Just "") (not . followsExp . lexeme)
      return Ast.ExprError
  where
    followsExp L.Semi = True
    followsExp L.RParen = True
    followsExp L.RBrack = True
    followsExp L.Comma = True
    followsExp _ = False

expr :: Parser Ast.Expr
expr = assignLevelExpr

assignLevelExpr :: Parser Ast.Expr
assignLevelExpr = logicalOrLevelExpr `chainr1` binop assign
  where
    assign = [(L.Assign, Ast.Assign)]

logicalOrLevelExpr :: Parser Ast.Expr
logicalOrLevelExpr = logicalAndLevelExpr `chainl1` binop logicorop
  where
    logicorop =
      [(L.Or, (`Ast.Binop` Ast.Or))]

logicalAndLevelExpr :: Parser Ast.Expr
logicalAndLevelExpr = bitwiseOrLevelExpr `chainl1` binop logicandop
  where
    logicandop =
      [(L.And, (`Ast.Binop` Ast.And))]

bitwiseOrLevelExpr :: Parser Ast.Expr
bitwiseOrLevelExpr = xorLevelExpr `chainl1` binop bitorop
  where
    bitorop =
      [(L.Bar, (`Ast.Binop` Ast.BitwiseOr))]

xorLevelExpr :: Parser Ast.Expr
xorLevelExpr = bitwiseAndLevelExpr `chainl1` binop bitxorop
  where
    bitxorop =
      [(L.Caret, (`Ast.Binop` Ast.BitwiseXor))]

bitwiseAndLevelExpr :: Parser Ast.Expr
bitwiseAndLevelExpr = eqLevelExpr `chainl1` binop bitandop
  where
    bitandop =
      [(L.Ampers, (`Ast.Binop` Ast.BitwiseAnd))]

eqLevelExpr :: Parser Ast.Expr
eqLevelExpr = compLevelExpr `chainl1` binop eqops
  where
    eqops =
      [ (L.Equal, (`Ast.Binop` Ast.Equal)),
        (L.Neq, (`Ast.Binop` Ast.Neq))
      ]

compLevelExpr :: Parser Ast.Expr
compLevelExpr = addLevelExpr `chainl1` binop compops
  where
    compops =
      [ (L.Less, (`Ast.Binop` Ast.Less)),
        (L.Leq, (`Ast.Binop` Ast.Leq)),
        (L.Greater, (`Ast.Binop` Ast.Greater)),
        (L.Geq, (`Ast.Binop` Ast.Geq))
      ]

addLevelExpr :: Parser Ast.Expr
addLevelExpr = multLevelExpr `chainl1` binop addops
  where
    addops =
      [ (L.Plus, (`Ast.Binop` Ast.Add)),
        (L.Minus, (`Ast.Binop` Ast.Sub))
      ]

multLevelExpr :: Parser Ast.Expr
multLevelExpr = castLevelExpr `chainl1` binop mulops
  where
    mulops =
      [ (L.Asterisk, (`Ast.Binop` Ast.Mul)),
        (L.Div, (`Ast.Binop` Ast.Div)),
        (L.Mod, (`Ast.Binop` Ast.Mod))
      ]

binop :: [(L.Lexeme, Ast.Expr -> Ast.Expr -> Ast.Expr)] -> Parser (Ast.Expr -> Ast.Expr -> Ast.Expr)
binop ops = choice [expect op >> return action | (op, action) <- ops]

castLevelExpr :: Parser Ast.Expr
castLevelExpr = try typecast <|> unaryLevelExpr

typecast :: Parser Ast.Expr
typecast = do
  typ <- between (expect L.LParen) (expect L.RParen) type'
  Ast.Typecast typ <$> castLevelExpr

unaryLevelExpr :: Parser Ast.Expr
unaryLevelExpr =
  negative
    <|> negate
    <|> deref
    <|> addressOf
    <|> sizeof
    <|> callLevelExpr

sizeof :: Parser Ast.Expr
sizeof = do
  expect L.Sizeof
  between (expect L.LParen) (expect L.RParen) sizeOfArg
  where
    sizeOfArg = (do Ast.Sizeof . Left <$> type') <|> (do Ast.Sizeof . Right <$> expr)

addressOf :: Parser Ast.Expr
addressOf = do
  expect L.Ampers
  Ast.AddressOf <$> unaryLevelExpr

deref :: Parser Ast.Expr
deref = do
  expect L.Asterisk
  Ast.Deref <$> unaryLevelExpr

negative :: Parser Ast.Expr
negative = do
  expect L.Minus
  Ast.Negative <$> unaryLevelExpr

negate :: Parser Ast.Expr
negate = do
  expect L.Not
  Ast.Negate <$> unaryLevelExpr

callLevelExpr :: Parser Ast.Expr
callLevelExpr =
  baseExprOrFuncCall
    `chainl1'` ( fieldAccess
                   <|> arrayAccess
                   <|> indirect
               )

baseExprOrFuncCall :: Parser Ast.Expr
baseExprOrFuncCall = do
  base <- baseExpr
  case base of
    (Ast.Ident func) -> funcCall func <|> return base
    _ -> return base

funcCall :: String -> Parser Ast.Expr
funcCall func = do
  expect LParen
  actuals <- actuals
  expect RParen
  return (Ast.Call func actuals)

indirect :: Parser (Ast.Expr -> Ast.Expr)
indirect = do
  expect L.Arrow
  id <- ident
  return (\left -> Ast.Indirect left (field id))
  where
    field (Ast.Ident x) = x

arrayAccess :: Parser (Ast.Expr -> Ast.Expr)
arrayAccess = do
  index <- between (expect L.LBrack) (expect L.RBrack) expr
  return (`Ast.ArrayAccess` index)

fieldAccess :: Parser (Ast.Expr -> Ast.Expr)
fieldAccess = do
  expect L.Dot
  id <- ident
  return (\left -> Ast.FieldAccess left (field id))
  where
    field (Ast.Ident x) = x

baseExpr :: Parser Ast.Expr
baseExpr =
  litInt
    <|> litDouble
    <|> litString
    <|> litChar
    <|> litNull
    <|> ident
    <|> between (expect L.LParen) (expect L.RParen) expr

ident :: Parser Ast.Expr
ident = token test Set.empty <?> "identifier"
  where
    test (Token (L.Ident name) _ _ _) = Just (Ast.Ident name)
    test _ = Nothing

end :: Parser L.Lexeme
end = expect L.Eof

litInt :: Parser Ast.Expr
litInt = token test Set.empty <?> "integer literal"
  where
    test (Token (L.LitInt n) _ _ _) = Just (Ast.LitInt n)
    test _ = Nothing

litDouble :: Parser Ast.Expr
litDouble = token test Set.empty <?> "double literal"
  where
    test (Token (L.LitDouble n) _ _ _) = Just (Ast.LitDouble n)
    test _ = Nothing

litString :: Parser Ast.Expr
litString = token test Set.empty <?> "string literal"
  where
    test (Token (L.LitString str) _ _ _) = Just (Ast.LitString str)
    test _ = Nothing

litChar :: Parser Ast.Expr
litChar = token test Set.empty <?> "char literal"
  where
    test (Token (L.LitChar char) _ _ _) = Just (Ast.LitChar char)
    test _ = Nothing

litNull :: Parser Ast.Expr
litNull = expect L.LitNull >> return Ast.Null <?> "NULL"

type' :: Parser Ast.Type
type' = primitiveType <|> structType

primitiveType :: Parser Ast.Type
primitiveType = do
  typeTok <- satisfy (L.isType . T.lexeme)
  Ast.PrimitiveType (getType typeTok) <$> stars
  where
    getType (T.Token (Type typ) _ _ _) = typ

structType :: Parser Ast.Type
structType = do
  expect L.Struct
  id <- ident
  Ast.StructType (structName id) <$> stars
  where
    structName (Ast.Ident x) = x

formals :: Parser [Ast.Formal]
formals = sepBy formal (expect Comma)

formal :: Parser Ast.Formal
formal = do
  typ <- type'
  Ast.Formal typ . formalName <$> ident
  where
    formalName (Ast.Ident x) = x

actuals :: Parser [Ast.Expr]
actuals = sepBy recoverExp (expect L.Comma)

arraySizes :: Parser [Int]
arraySizes = many (between (expect L.LBrack) (expect L.RBrack) size)
  where
    size = getSize <$> litInt
    getSize (Ast.LitInt x) = x

stars :: Parser Int
stars = do
  xs <- many (expect L.Asterisk)
  return (Prelude.length xs)

include :: Parser Ast.Directive
include = do
  expect L.Include
  Ast.Include . file <$> litString
  where
    file (Ast.LitString name) = name

includes :: Parser [Ast.Directive]
includes = many include

-- streamify :: String -> TokenStream
-- streamify input = case lex' "" input of
--   (Left error) -> TokenStream {tokenStreamInput = input, unTokenStream = []}
--   (Right stream) -> TokenStream {tokenStreamInput = input, unTokenStream = stream}

stream :: String -> [T.Token] -> TokenStream
stream input tokens =
  TokenStream
    { tokenStreamInput = input,
      unTokenStream = tokens
    }

parse :: String -> [T.Token] -> Either (ParseErrorBundle TokenStream Void) Ast.Program
parse input tokens = Text.Megaparsec.parse program "" $ stream input tokens

-- parse' :: String -> [T.Token] -> IO ()
-- parse' input tokens = case Parser.Combinator.Parser.parse input tokens of
--   (Left error) -> putStrLn (errorBundlePretty error)
--   (Right ast) -> putStrLn $ visualiseAst ast