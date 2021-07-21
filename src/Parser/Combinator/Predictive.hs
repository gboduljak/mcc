{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Parser.Combinator.Predictive where

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
import Parser.Combinator.Prim
import Parser.Combinator.TokenStream
import Parser.Combinator.When
import Text.Megaparsec
import Prelude hiding (negate)

predChainr1 :: Parser a -> [(L.Lexeme -> Bool, Parser (a -> a -> a))] -> Parser a
predChainr1 p opts = do left <- p; rest left
  where
    rest left =
      look >>= \lexeme -> do
        let ops = [op | (pred, op) <- opts, pred lexeme]
        case ops of
          [] -> return left
          op : _ -> do
            f <- op
            right <- predChainr1 p opts
            rest (f left right)

predChainl1 :: Parser a -> [(L.Lexeme -> Bool, Parser (a -> a -> a))] -> Parser a
predChainl1 p opts = do left <- p; rest left
  where
    rest left =
      look >>= \lexeme -> do
        let ops = [op | (pred, op) <- opts, pred lexeme]
        case ops of
          [] -> return left
          op : _ -> do
            f <- op
            right <- p
            rest (f left right)

predChainl1' :: Parser a -> [(L.Lexeme -> Bool, Parser (a -> a))] -> Parser a
predChainl1' p opts = do left <- p; rest left
  where
    rest left =
      look >>= \lexeme -> do
        let ops = [op | (pred, op) <- opts, pred lexeme]
        case ops of
          [] -> return left
          op : _ -> do
            f <- op
            rest (f left)

program :: Parser Ast.Program
program = do
  dirs <- directives
  Ast.Program dirs <$> constructs

directives :: Parser [Ast.Directive]
directives = includes

constructs :: Parser [Ast.Construct]
constructs = many construct

construct :: Parser Ast.Construct
construct =
  recoverUsingFollows
    ( (>?)
        [ (L.is Struct, structDecl),
          (const True, funcDefOrFuncDefOrVarDecl)
        ]
    )
    followsConstruct
    (return Ast.ConstructError)

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

funcDefOrFuncDefOrVarDecl :: Parser Ast.Construct
funcDefOrFuncDefOrVarDecl = do
  typ <- type'
  nameId <- ident
  (>?)
    [ (L.is LParen, func' typ (name nameId)),
      (L.is LBrack, varDecl' typ (name nameId)),
      (L.is Semi, varDecl' typ (name nameId))
    ]
  where
    func' typ name = do
      args <- between (expect L.LParen) (expect L.RParen) formals
      (>?)
        [ (L.is Semi, Ast.FuncDecl <$> funcDecl typ name args),
          (L.is LBrace, Ast.FuncDefn <$> funcDefn typ name args)
        ]
    varDecl' typ name = Ast.VarDecl <$> varDecl typ name
    name (Ast.Ident x) = x

funcDecl :: Ast.Type -> String -> [Ast.Formal] -> Parser Ast.FuncDecl
funcDecl rettyp funcName formals = do
  expect L.Semi
  return (Ast.Func rettyp funcName formals)

funcDefn :: Ast.Type -> String -> [Ast.Formal] -> Parser Ast.FuncDef
funcDefn rettyp funcName formals = do
  Ast.FuncDef rettyp funcName formals <$> block

varDecl :: Ast.Type -> String -> Parser Ast.VarDecl
varDecl typ name = do
  sizes <- arraySizes
  expect Semi
  return (Ast.Var typ name sizes)

statement :: Parser Ast.Statement
statement =
  recoverUsingFollows
    ( (>?)
        [ (L.is L.While, while),
          (L.is L.For, for),
          (L.is L.If, if'),
          (L.is L.Return, returnStmt),
          (L.is L.LBrace, blockStmt),
          (L.isType, varDeclStmt),
          (startsExpr, exprStmt)
        ]
    )
    followsStatement
    (return Ast.StatementError)
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

while :: Parser Ast.Statement
while = do
  expect L.While
  cond <- between (expect L.LParen) (expect L.RParen) expr
  Ast.While cond <$> statement

for :: Parser Ast.Statement
for = do
  expect L.For
  expect L.LParen
  init <- optionalExpr
  expect L.Semi
  cond <- optionalExpr
  expect L.Semi
  incr <- optionalExpr
  expect L.RParen
  Ast.For init cond incr <$> statement

if' :: Parser Ast.Statement
if' = do
  expect L.If
  cond <- between (expect L.LParen) (expect L.RParen) expr
  conseq <- statement
  Ast.If cond conseq <$> alt
  where
    alt =
      look >>= \case
        L.Else -> expect L.Else >> Just <$> statement
        _ -> return Nothing

return' :: Parser Ast.Statement
return' = do
  expect L.Return
  Ast.Return <$> optionalExpr

block :: Parser Ast.Block
block = do
  expect L.LBrace
  stmts <- manyTill statement (expect L.RBrace)
  return (Ast.Block stmts)

recoverUsingFollows :: Parser a -> (L.Lexeme -> Bool) -> Parser a -> Parser a
recoverUsingFollows p f e = do
  withRecovery
    ( \error -> do
        registerParseError error
        takeWhile1P (Just "") (not . f . lexeme)
        e
    )
    p

startsExpr :: Lexeme -> Bool
startsExpr lexeme =
  L.any [LParen, Minus, Not, Asterisk, Ampers, Sizeof, LitNull] lexeme
    || L.isLit lexeme
    || L.isIdent lexeme

optionalExpr :: Parser (Maybe Ast.Expr)
optionalExpr =
  (>?)
    [ (startsExpr, Just <$> expr),
      (const True, return Nothing)
    ]

expr :: Parser Ast.Expr
expr = recoverUsingFollows assignLevelExpr followsExp (return Ast.ExprError)

assignLevelExpr :: Parser Ast.Expr
assignLevelExpr = logicalOrLevelExpr `predChainr1` binop assign
  where
    assign = [(L.Assign, Ast.Assign)]

logicalOrLevelExpr :: Parser Ast.Expr
logicalOrLevelExpr = logicalAndLevelExpr `predChainl1` binop logicorop
  where
    logicorop =
      [(L.Or, (`Ast.Binop` Ast.Or))]

logicalAndLevelExpr :: Parser Ast.Expr
logicalAndLevelExpr = bitwiseOrLevelExpr `predChainl1` binop logicandop
  where
    logicandop =
      [(L.And, (`Ast.Binop` Ast.And))]

bitwiseOrLevelExpr :: Parser Ast.Expr
bitwiseOrLevelExpr = xorLevelExpr `predChainl1` binop bitorop
  where
    bitorop =
      [(L.Bar, (`Ast.Binop` Ast.BitwiseOr))]

xorLevelExpr :: Parser Ast.Expr
xorLevelExpr = bitwiseAndLevelExpr `predChainl1` binop bitxorop
  where
    bitxorop =
      [(L.Caret, (`Ast.Binop` Ast.BitwiseXor))]

bitwiseAndLevelExpr :: Parser Ast.Expr
bitwiseAndLevelExpr = eqLevelExpr `predChainl1` binop bitandop
  where
    bitandop =
      [(L.Ampers, (`Ast.Binop` Ast.BitwiseAnd))]

eqLevelExpr :: Parser Ast.Expr
eqLevelExpr = compLevelExpr `predChainl1` binop eqops
  where
    eqops =
      [ (L.Equal, (`Ast.Binop` Ast.Equal)),
        (L.Neq, (`Ast.Binop` Ast.Neq))
      ]

compLevelExpr :: Parser Ast.Expr
compLevelExpr = addLevelExpr `predChainl1` binop compops
  where
    compops =
      [ (L.Less, (`Ast.Binop` Ast.Less)),
        (L.Leq, (`Ast.Binop` Ast.Leq)),
        (L.Greater, (`Ast.Binop` Ast.Greater)),
        (L.Geq, (`Ast.Binop` Ast.Geq))
      ]

addLevelExpr :: Parser Ast.Expr
addLevelExpr = multLevelExpr `predChainl1` binop addops
  where
    addops =
      [ (L.Plus, (`Ast.Binop` Ast.Add)),
        (L.Minus, (`Ast.Binop` Ast.Sub))
      ]

multLevelExpr :: Parser Ast.Expr
multLevelExpr = castLevelExpr `predChainl1` binop mulops
  where
    mulops =
      [ (L.Asterisk, (`Ast.Binop` Ast.Mul)),
        (L.Div, (`Ast.Binop` Ast.Div)),
        (L.Mod, (`Ast.Binop` Ast.Mod))
      ]

binop ::
  [(L.Lexeme, Ast.Expr -> Ast.Expr -> Ast.Expr)] ->
  [(L.Lexeme -> Bool, Parser (Ast.Expr -> Ast.Expr -> Ast.Expr))]
binop ops = [(L.is op, expect op >> return action) | (op, action) <- ops]

castLevelExpr :: Parser Ast.Expr
castLevelExpr =
  (>?)
    [ (L.is LParen, castTypeOrExp),
      (const True, unaryLevelExpr)
    ]

castTypeOrExp :: Parser Ast.Expr
castTypeOrExp = do
  expect L.LParen
  (>?)
    [ (L.isType, castType),
      (const True, nestedExpr)
    ]
  where
    castType = do
      typ <- type'
      expect L.RParen
      Ast.Typecast typ <$> castLevelExpr
    nestedExpr = do
      exp <- expr
      expect L.RParen
      return (Ast.Nested exp)

unaryLevelExpr :: Parser Ast.Expr
unaryLevelExpr =
  (>?)
    [ (L.is Minus, negative),
      (L.is Not, negate),
      (L.is Asterisk, deref),
      (L.is Ampers, addressOf),
      (L.is Sizeof, sizeof),
      (const True, callLevelExpr)
    ]

sizeof :: Parser Ast.Expr
sizeof = do
  expect L.Sizeof
  between (expect L.LParen) (expect L.RParen) sizeOfArg
  where
    sizeOfArg =
      (>?)
        [ (L.isType, Ast.Sizeof . Left <$> type'),
          (const True, Ast.Sizeof . Right <$> expr)
        ]

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
callLevelExpr = do
  baseExprOrFuncCall
    `predChainl1'` [ (L.is Dot, fieldAccess),
                     (L.is LBrack, arrayAccess),
                     (L.is Arrow, indirect)
                   ]

baseExprOrFuncCall :: Parser Ast.Expr
baseExprOrFuncCall = do
  base <- baseExpr
  case base of
    (Ast.Ident func) -> do
      look >>= \case
        LParen -> funcCall func
        _ -> return base
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
  (>?)
    [ (isLitInt, litInt),
      (isLitDouble, litDouble),
      (isLitString, litString),
      (isLitChar, litChar),
      (L.is L.LitNull, litNull),
      (L.isIdent, ident),
      (L.is L.LParen, Ast.Nested <$> between (expect L.LParen) (expect L.RParen) expr)
    ]

ident :: Parser Ast.Expr
ident = token test Set.empty <?> "identifier"
  where
    test (Token (L.Ident name) _ _ _) = Just (Ast.Ident name)
    test _ = Nothing

type' :: Parser Ast.Type
type' =
  (>?)
    [ (L.isType, primitiveType),
      (L.is Struct, structType)
    ]

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

stars :: Parser Int
stars = do
  xs <- many (expect L.Asterisk)
  return (Prelude.length xs)

arraySizes :: Parser [Int]
arraySizes = many (between (expect L.LBrack) (expect L.RBrack) size)
  where
    size = getSize <$> litInt
    getSize (Ast.LitInt x) = x

actuals :: Parser [Ast.Expr]
actuals = sepBy expr (expect L.Comma)

formals :: Parser [Ast.Formal]
formals = sepBy formal (expect L.Comma)

formal :: Parser Ast.Formal
formal = do
  typ <- type'
  Ast.Formal typ . formalName <$> ident
  where
    formalName (Ast.Ident x) = x

include :: Parser Ast.Directive
include = do
  expect L.Include
  Ast.Include . file <$> litString
  where
    file (Ast.LitString name) = name

includes :: Parser [Ast.Directive]
includes = many include

followsExp :: Lexeme -> Bool
followsExp L.Semi = True
followsExp L.RParen = True
followsExp L.RBrack = True
followsExp L.Comma = True
followsExp _ = False

followsStatement :: Lexeme -> Bool
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

followsConstruct :: Lexeme -> Bool
followsConstruct L.Struct = True
followsConstruct (L.Type _) = True
followsConstruct (L.Eof) = True
followsConstruct _ = False

-- streamify :: String -> TokenStream
-- streamify input = case lex' "" input of
--   (Left error) -> TokenStream {tokenStreamInput = input, unTokenStream = []}
--   (Right stream) -> TokenStream {tokenStreamInput = input, unTokenStream = stream}

-- parse'' input tokens = Text.Megaparsec.parse baseExpr "" $ stream input tokens

-- parseComb input = case Text.Megaparsec.parse statement "" (streamify input) of
--   (Left error) -> putStrLn (errorBundlePretty error)
--   (Right ast) -> putStrLn $ visualiseAst ast

stream :: String -> [T.Token] -> TokenStream
stream input tokens =
  TokenStream
    { tokenStreamInput = input,
      unTokenStream = tokens
    }

parse :: String -> [T.Token] -> Either (ParseErrorBundle TokenStream Void) Ast.Program
parse input tokens = Text.Megaparsec.parse program "" $ stream input tokens
