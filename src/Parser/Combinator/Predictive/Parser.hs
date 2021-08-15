{-# LANGUAGE LambdaCase #-}

module Parser.Combinator.Predictive.Parser where

import qualified Data.List as DL
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Set as Set
import Data.Void
import Lexer.Combinator.Lexer (lex')
import Lexer.Lexeme as L
import Lexer.Token as T
import Parser.Ast (getExprOff, isPointer, isPrimitive)
import qualified Parser.Ast as Ast
import Parser.AstVisualiser (visualiseAst)
import Parser.Combinator.CustomCombinators.Chains
import Parser.Combinator.CustomCombinators.Recover
import Parser.Combinator.CustomCombinators.When
import Parser.Combinator.Prim
import Parser.Combinator.TokenStream
import Parser.Grammar.Firsts (startsExpr, startsType)
import Parser.Grammar.Follows
import Text.Megaparsec
import Prelude hiding (negate)

program :: Parser Ast.Program
program = do
  dirs <- directives
  constrs <- constructs
  end
  return (Ast.Program dirs constrs)

directives :: Parser [Ast.Directive]
directives = includes

constructs :: Parser [Ast.Construct]
constructs = many construct

construct :: Parser Ast.Construct
construct =
  recoverUsingFollows
    ( do
        off <- getOffset
        typ <- type'
        if isPrimitive typ
          then do
            name <- nameFrom <$> ident
            funcDefOrFuncDeclOrVarDecl typ name
          else do
            if isPointer typ
              then do
                name <- nameFrom <$> ident
                funcDefOrFuncDeclOrVarDecl typ name
              else
                (>?)
                  [ (L.is L.LBrace, structDecl off (structName typ)),
                    (L.isIdent, ident >>= funcDefOrFuncDeclOrVarDecl typ . nameFrom)
                  ]
    )
    followsConstruct
    (return Ast.ConstructError)
  where
    nameFrom (Ast.Ident x _) = x
    nameFrom _ = undefined
    structName (Ast.StructType x _) = x
    structName _ = undefined 
    
structDecl :: Int -> String -> Parser Ast.Construct
structDecl off name = do
  vars <- structFields off
  expect Semi
  return (Ast.StructDecl $ Ast.Struct name vars off)
  where
    structName (Ast.Ident x _) = x
    structName _ = undefined

structFields :: Int -> Parser [Ast.VarDecl]
structFields off = do
  expect L.LBrace
  manyTill recField (expect L.RBrace)
  where
    recField = recoverUsingFollows field followsStructField (return (Ast.VarDeclError off))
    field = do
      typ <- type'
      nameId <- ident
      varDecl off typ (name nameId)
    name (Ast.Ident x _) = x
    name _ = undefined

funcDefOrFuncDeclOrVarDecl :: Ast.Type -> String -> Parser Ast.Construct
funcDefOrFuncDeclOrVarDecl typ name = do
  off <- getOffset
  (>?)
    [ (L.is LParen, func' off typ name),
      (L.is LBrack, varDecl' off typ name),
      (L.is Semi, varDecl' off typ name)
    ]
  where
    func' off typ name = do
      args <- between (expect L.LParen) (expect L.RParen) formals
      (>?)
        [ (L.is Semi, Ast.FuncDecl <$> funcDecl off typ name args),
          (L.is LBrace, Ast.FuncDefn <$> funcDefn off typ name args)
        ]
    varDecl' off typ name = Ast.VarDecl <$> varDecl off typ name

funcDecl :: Int -> Ast.Type -> String -> [Ast.Formal] -> Parser Ast.FuncDecl
funcDecl off rettyp funcName formals = do
  expect L.Semi
  return (Ast.Func rettyp funcName formals off)

funcDefn :: Int -> Ast.Type -> String -> [Ast.Formal] -> Parser Ast.FuncDef
funcDefn off rettyp funcName formals = do
  block' <- block
  return (Ast.FuncDef rettyp funcName formals block' off)

varDecl :: Int -> Ast.Type -> String -> Parser Ast.VarDecl
varDecl off typ name = do
  sizes <- arraySizes
  expect Semi
  return (Ast.Var typ name sizes off)

statement :: Parser Ast.Statement
statement = do
  off <- getOffset
  recoverUsingFollows
    ( (>?)
        [ (L.is L.While, while),
          (L.is L.For, for),
          (L.is L.If, if'),
          (L.is L.Return, returnStmt),
          (L.is L.LBrace, blockStmt),
          (startsType, varDeclStmt),
          (startsExpr, exprStmt)
        ]
    )
    followsStatement
    (return $ Ast.StatementError off)
  where
    exprStmt = do exp <- expr; expect Semi; return (Ast.Expr exp (getExprOff exp))
    blockStmt = do block' <- block; Ast.BlockStatement block' <$> getOffset
    returnStmt = do ret <- return'; expect Semi; return ret
    varDeclStmt = do
      off <- getOffset
      typ <- type'
      nameId <- ident
      decl <- varDecl off typ (varName nameId)
      return (Ast.VarDeclStatement decl off)
      where
        varName (Ast.Ident name _) = name
        varName _ = undefined

while :: Parser Ast.Statement
while = do
  off <- getOffset
  expect L.While
  cond <- between (expect L.LParen) (expect L.RParen) expr
  body <- statement
  return (Ast.While cond body off)

for :: Parser Ast.Statement
for = do
  off <- getOffset
  expect L.For
  expect L.LParen
  init <- optionalExpr
  expect L.Semi
  cond <- optionalExpr
  expect L.Semi
  incr <- optionalExpr
  expect L.RParen
  body <- statement
  return (Ast.For init cond incr body off)

if' :: Parser Ast.Statement
if' = do
  off <- getOffset
  expect L.If
  cond <- between (expect L.LParen) (expect L.RParen) expr
  conseq <- statement
  alt' <- alt
  return (Ast.If cond conseq alt' off)
  where
    alt =
      look >>= \case
        L.Else -> expect L.Else >> Just <$> statement
        _ -> return Nothing

return' :: Parser Ast.Statement
return' = do
  off <- getOffset
  expect L.Return
  expr <- optionalExpr
  return (Ast.Return expr off)

block :: Parser Ast.Block
block = do
  off <- getOffset
  expect L.LBrace
  stmts <- manyTill statement (expect L.RBrace)
  return (Ast.Block stmts off)

optionalExpr :: Parser (Maybe Ast.Expr)
optionalExpr =
  (>?)
    [ (startsExpr, Just <$> expr),
      (const True, return Nothing)
    ]

expr :: Parser Ast.Expr
expr = do off <- getOffset; recoverUsingFollows assignLevelExpr followsExp (return (Ast.ExprError off))

assignLevelExpr :: Parser Ast.Expr
assignLevelExpr = logicalOrLevelExpr `lookchainr1` binop assign
  where
    assign = [(L.Assign, \left right -> Ast.Assign left right (getExprOff left))]

logicalOrLevelExpr :: Parser Ast.Expr
logicalOrLevelExpr = logicalAndLevelExpr `lookchainl1` binop logicorop
  where
    logicorop =
      [(L.Or, \left right -> Ast.Binop left Ast.Or right (getExprOff left))]

logicalAndLevelExpr :: Parser Ast.Expr
logicalAndLevelExpr = bitwiseOrLevelExpr `lookchainl1` binop logicandop
  where
    logicandop =
      [(L.And, \left right -> Ast.Binop left Ast.And right (getExprOff left))]

bitwiseOrLevelExpr :: Parser Ast.Expr
bitwiseOrLevelExpr = xorLevelExpr `lookchainl1` binop bitorop
  where
    bitorop =
      [(L.Bar, \left right -> Ast.Binop left Ast.BitwiseOr right (getExprOff left))]

xorLevelExpr :: Parser Ast.Expr
xorLevelExpr = bitwiseAndLevelExpr `lookchainl1` binop bitxorop
  where
    bitxorop =
      [(L.Caret, \left right -> Ast.Binop left Ast.BitwiseXor right (getExprOff left))]

bitwiseAndLevelExpr :: Parser Ast.Expr
bitwiseAndLevelExpr = eqLevelExpr `lookchainl1` binop bitandop
  where
    bitandop =
      [(L.Ampers, \left right -> Ast.Binop left Ast.BitwiseAnd right (getExprOff left))]

eqLevelExpr :: Parser Ast.Expr
eqLevelExpr = compLevelExpr `lookchainl1` binop eqops
  where
    eqops =
      [ (L.Equal, \left right -> Ast.Binop left Ast.Equal right (getExprOff left)),
        (L.Neq, \left right -> Ast.Binop left Ast.Neq right (getExprOff left))
      ]

compLevelExpr :: Parser Ast.Expr
compLevelExpr = addLevelExpr `lookchainl1` binop compops
  where
    compops =
      [ (L.Less, \left right -> Ast.Binop left Ast.Less right (getExprOff left)),
        (L.Leq, \left right -> Ast.Binop left Ast.Leq right (getExprOff left)),
        (L.Greater, \left right -> Ast.Binop left Ast.Greater right (getExprOff left)),
        (L.Geq, \left right -> Ast.Binop left Ast.Geq right (getExprOff left))
      ]

addLevelExpr :: Parser Ast.Expr
addLevelExpr = multLevelExpr `lookchainl1` binop addops
  where
    addops =
      [ (L.Plus, \left right -> Ast.Binop left Ast.Add right (getExprOff left)),
        (L.Minus, \left right -> Ast.Binop left Ast.Sub right (getExprOff left))
      ]

multLevelExpr :: Parser Ast.Expr
multLevelExpr = castLevelExpr `lookchainl1` binop mulops
  where
    mulops =
      [ (L.Asterisk, \left right -> Ast.Binop left Ast.Mul right (getExprOff left)),
        (L.Div, \left right -> Ast.Binop left Ast.Div right (getExprOff left)),
        (L.Mod, \left right -> Ast.Binop left Ast.Mod right (getExprOff left))
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
  off <- getOffset
  expect L.LParen
  (>?)
    [ (startsType, castType off),
      (startsExpr, nestedExpr off)
    ]
  where
    castType off = do
      typ <- type'
      expect L.RParen
      expr <- castLevelExpr
      return (Ast.Typecast typ expr off)
    nestedExpr off = do
      exp <- expr
      expect L.RParen
      return (Ast.Nested exp off)

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
  off <- getOffset
  expect L.Sizeof
  between (expect L.LParen) (expect L.RParen) (sizeOfArg off)
  where
    sizeOfArg off =
      (>?)
        [ (startsType, do typ <- sizeofType; return (Ast.Sizeof (Left typ) off)),
          (const True, do exp <- expr; return (Ast.Sizeof (Right exp) off))
        ]
        
sizeofType :: Parser Ast.SizeofType
sizeofType = do
  typ <- type'
  Ast.SizeofType typ <$> arraySizes

addressOf :: Parser Ast.Expr
addressOf = do
  off <- getOffset
  expect L.Ampers
  expr <- unaryLevelExpr
  return (Ast.AddressOf expr off)

deref :: Parser Ast.Expr
deref = do
  off <- getOffset
  expect L.Asterisk
  expr <- unaryLevelExpr
  return (Ast.Deref expr off)

negative :: Parser Ast.Expr
negative = do
  off <- getOffset
  expect L.Minus
  expr <- unaryLevelExpr
  return (Ast.Negative expr off)

negate :: Parser Ast.Expr
negate = do
  off <- getOffset
  expect L.Not
  expr <- unaryLevelExpr
  return (Ast.Negate expr off)

callLevelExpr :: Parser Ast.Expr
callLevelExpr =
  baseExprOrFuncCall
    `lookchainl1'` [ (L.is Dot, fieldAccess),
                     (L.is LBrack, arrayAccess),
                     (L.is Arrow, indirect),
                     (L.is Increment, increment),
                     (L.is Decrement, decrement)
                   ]

baseExprOrFuncCall :: Parser Ast.Expr
baseExprOrFuncCall = do
  base <- baseExpr
  case base of
    (Ast.Ident func _) ->
      look >>= \case
        LParen -> funcCall func (getExprOff base)
        _ -> return base
    _ -> return base

funcCall :: String -> Int -> Parser Ast.Expr
funcCall func off = do
  expect LParen
  actuals <- actuals
  expect RParen
  return (Ast.Call func actuals off)

increment :: Parser (Ast.Expr -> Ast.Expr)
increment = do 
  expect L.Increment
  return (\left -> Ast.Increment left (getExprOff left))

decrement :: Parser (Ast.Expr -> Ast.Expr)
decrement = do 
  expect L.Decrement
  return (\left -> Ast.Decrement left (getExprOff left))

indirect :: Parser (Ast.Expr -> Ast.Expr)
indirect = do
  expect L.Arrow
  id <- ident
  return (\left -> Ast.Indirect left (field id) (getExprOff left))
  where
    field (Ast.Ident x _) = x
    field _ = undefined

arrayAccess :: Parser (Ast.Expr -> Ast.Expr)
arrayAccess = do
  index <- between (expect L.LBrack) (expect L.RBrack) expr
  return (\left -> Ast.ArrayAccess left index (getExprOff left))

fieldAccess :: Parser (Ast.Expr -> Ast.Expr)
fieldAccess = do
  expect L.Dot
  id <- ident
  return (\left -> Ast.FieldAccess left (field id) (getExprOff left))
  where
    field (Ast.Ident x _) = x
    field _ = undefined

baseExpr :: Parser Ast.Expr
baseExpr =
  (>?)
    [ (isLitInt, litInt),
      (isLitDouble, litDouble),
      (isLitString, litString),
      (isLitChar, litChar),
      (L.is L.LitNull, litNull),
      (L.isIdent, ident),
      ( L.is L.LParen,
        do
          off <- getOffset
          exp <- between (expect L.LParen) (expect L.RParen) expr
          return (Ast.Nested exp off)
      )
    ]

ident :: Parser Ast.Expr
ident = do off <- getOffset; token (test off) Set.empty <?> "identifier"
  where
    test off (Token (L.Ident name) _ _ _) = Just (Ast.Ident name off)
    test _ _ = Nothing

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
    getType _ = undefined

structType :: Parser Ast.Type
structType = do
  expect L.Struct
  id <- ident
  Ast.StructType (structName id) <$> stars
  where
    structName (Ast.Ident x _) = x
    structName _ = undefined

end :: Parser L.Lexeme
end = expect L.Eof

litInt :: Parser Ast.Expr
litInt = do off <- getOffset; token (test off) Set.empty <?> "integer literal"
  where
    test off (Token (L.LitInt n) _ _ _) = Just (Ast.LitInt n off)
    test _ _ = Nothing

litDouble :: Parser Ast.Expr
litDouble = do off <- getOffset; token (test off) Set.empty <?> "double literal"
  where
    test off (Token (L.LitDouble n) _ _ _) = Just (Ast.LitDouble n off)
    test _ _ = Nothing

litString :: Parser Ast.Expr
litString = do off <- getOffset; token (test off) Set.empty <?> "string literal"
  where
    test off (Token (L.LitString str) _ _ _) = Just (Ast.LitString str off)
    test _ _ = Nothing

litChar :: Parser Ast.Expr
litChar = do off <- getOffset; token (test off) Set.empty <?> "char literal"
  where
    test off (Token (L.LitChar char) _ _ _) = Just (Ast.LitChar char off)
    test _ _ = Nothing

litNull :: Parser Ast.Expr
litNull = do off <- getOffset; expect L.LitNull >> return (Ast.Null off) <?> "NULL"

stars :: Parser Int
stars = do
  xs <- many (expect L.Asterisk)
  return (Prelude.length xs)

arraySizes :: Parser [Int]
arraySizes = many (between (expect L.LBrack) (expect L.RBrack) size)
  where
    size = getSize <$> litInt
    getSize (Ast.LitInt x _) = x
    getSize _ = undefined

actuals :: Parser [Ast.Expr]
actuals = sepBy expr (expect L.Comma)

formals :: Parser [Ast.Formal]
formals = sepBy formal (expect L.Comma)

formal :: Parser Ast.Formal
formal = do
  off <- getOffset
  typ <- type'
  ident' <- ident
  return (Ast.Formal typ (formalName ident') off)
  where
    formalName (Ast.Ident x _) = x
    formalName _ = undefined

include :: Parser Ast.Directive
include = do
  expect L.Include
  Ast.Include . file <$> litString
  where
    file (Ast.LitString name _) = name
    file _ = undefined

includes :: Parser [Ast.Directive]
includes = many include

stream :: String -> [T.Token] -> TokenStream
stream input tokens =
  TokenStream
    { tokenStreamInput = input,
      unTokenStream = tokens
    }

parse :: String -> [T.Token] -> Either (ParseErrorBundle TokenStream Void) Ast.Program
parse input tokens = Text.Megaparsec.parse program "" $ stream input tokens
