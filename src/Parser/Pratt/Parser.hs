{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Parser.Pratt.Parser where

import Control.Monad.Except (runExceptT)
import Control.Monad.State (evalState, runState)
import Data.List (intercalate, map, nub)
import qualified Data.List.NonEmpty as Ne
import Data.Text (pack)
import Data.Void
import Lexer.Combinator.Lexer (lex')
import qualified Lexer.Lexeme as L
import qualified Lexer.Token as T
import Parser.Ast (isPointer, isPrimitive)
import qualified Parser.Ast as Ast
import Parser.AstVisualiser (visualiseAst)
import Parser.Combinator.TokenStream (TokenStream (..))
import Parser.Errors.PrettyPrinter (prettyPrintErrors)
import Parser.Grammar.Firsts (startsExpr, startsStmt, startsType)
import Parser.Grammar.Follows (followsConstruct, followsExp, followsStatement)
import Parser.Grammar.Operators (isInfix, precedence)
import Parser.Pratt.Combinators.Chains (lookchainl1)
import Parser.Pratt.Combinators.Prim (many, sepBy, sepBy1)
import Parser.Pratt.Combinators.Recovery (withFollowsRecovery)
import Parser.Pratt.Combinators.When ((>?))
import Parser.Pratt.Prim (Parser, ParserError, ParserState (ParserState, errors), advance, expect, head, liftToken, lookahead, unexpected, unexpectedEof)
import System.Console.Pretty (supportsPretty)
import Text.Megaparsec (MonadParsec (lookAhead), ParseErrorBundle (ParseErrorBundle), PosState (PosState, pstateInput, pstateLinePrefix, pstateOffset, pstateSourcePos, pstateTabWidth), initialPos, mkPos, pos1)
import Prelude hiding (head)

program :: Parser Ast.Program
program = do
  dirs <- directives
  Ast.Program dirs <$> constructs

directives :: Parser [Ast.Directive]
directives = includes

constructs :: Parser [Ast.Construct]
constructs = many construct L.isType

construct :: Parser Ast.Construct
construct =
  withFollowsRecovery
    (lookahead >>= ctd)
    followsConstruct
    Ast.ConstructError

structDecl :: String -> Parser Ast.Construct
structDecl structName = do
  vars <- structFields
  expect (L.is L.Semi)
  return (Ast.StructDecl $ Ast.Struct structName vars)

structFields :: Parser [Ast.VarDecl]
structFields = do
  expect (L.is L.LBrace)
  fields <- sepBy1 structField startsType L.Semi
  expect (L.is L.RBrace)
  return fields

structField :: Parser Ast.VarDecl
structField =
  withFollowsRecovery
    ( do
        typ <- type'
        nameId <- expect L.isIdent
        varDecl typ (name nameId)
    )
    (L.any [L.RBrace, L.Semi])
    Ast.VarDeclError
  where
    name (L.Ident x) = x

funcDefOrFuncDeclOrVarDecl :: Ast.Type -> String -> Parser Ast.Construct
funcDefOrFuncDeclOrVarDecl typ name = do
  (>?)
    [ (L.is L.LParen, func' typ name),
      (L.is L.LBrack, globalVarDecl typ name),
      (L.is L.Semi, globalVarDecl typ name)
    ]
  where
    func' typ name = do
      args <- do
        expect (L.is L.LParen)
        args <- formals
        expect (L.is L.RParen)
        return args
      (>?)
        [ (L.is L.Semi, Ast.FuncDecl <$> funcDecl typ name args),
          (L.is L.LBrace, Ast.FuncDefn <$> funcDefn typ name args)
        ]

globalVarDecl :: Ast.Type -> String -> Parser Ast.Construct
globalVarDecl typ name = do
  decl <- Ast.VarDecl <$> varDecl typ name
  expect (L.is L.Semi)
  return decl

funcDecl :: Ast.Type -> String -> [Ast.Formal] -> Parser Ast.FuncDecl
funcDecl rettyp funcName formals = do
  expect (L.is L.Semi)
  return (Ast.Func rettyp funcName formals)

funcDefn :: Ast.Type -> String -> [Ast.Formal] -> Parser Ast.FuncDef
funcDefn rettyp funcName formals = do
  Ast.FuncDef rettyp funcName formals <$> block

statement :: Parser Ast.Statement
statement =
  withFollowsRecovery
    ( do
        stmt <- lookahead >>= std
        if followedBySemi stmt
          then expect (L.is L.Semi) >> return stmt
          else return stmt
    )
    followsStatement
    Ast.StatementError
  where
    followedBySemi Ast.While {} = False
    followedBySemi Ast.For {} = False
    followedBySemi Ast.If {} = False
    followedBySemi Ast.BlockStatement {} = False
    followedBySemi _ = True

block :: Parser Ast.Block
block = do
  expect (L.is L.LBrace)
  stmts <- many statement startsStmt
  expect (L.is L.RBrace)
  return (Ast.Block stmts)

localVarDecl :: Parser Ast.Statement
localVarDecl = do
  typ <- type'
  nameId <- expect L.isIdent
  Ast.VarDeclStatement <$> varDecl typ (name nameId)
  where
    name (L.Ident x) = x

varDecl :: Ast.Type -> String -> Parser Ast.VarDecl
varDecl typ name = Ast.Var typ name <$> arraySizes

while' :: Parser Ast.Statement
while' = do
  expect (L.is L.While)
  expect (L.is L.LParen)
  cond <- statementExpr
  expect (L.is L.RParen)
  Ast.While cond <$> statement

for' :: Parser Ast.Statement
for' = do
  expect (L.is L.For)
  expect (L.is L.LParen)
  init <- optionalStatementExpr
  expect (L.is L.Semi)
  cond <- optionalStatementExpr
  expect (L.is L.Semi)
  incr <- optionalStatementExpr
  expect (L.is L.RParen)
  Ast.For init cond incr <$> statement

if' :: Parser Ast.Statement
if' = do
  expect (L.is L.If)
  expect (L.is L.LParen)
  cond <- statementExpr
  expect (L.is L.RParen)
  conseq <- statement
  Ast.If cond conseq <$> alt
  where
    alt =
      (>?)
        [ (L.is L.Else, expect (L.is L.Else) >> Just <$> statement),
          (const True, return Nothing)
        ]

return' :: Parser Ast.Statement
return' = do
  expect (L.is L.Return)
  Ast.Return <$> optionalStatementExpr

optionalStatementExpr :: Parser (Maybe Ast.Expr)
optionalStatementExpr =
  (>?)
    [ (startsExpr, Just <$> statementExpr),
      (const True, return Nothing)
    ]

statementExpr :: Parser Ast.Expr
statementExpr = withFollowsRecovery (expr 0) followsExp Ast.ExprError

expr :: Int -> Parser Ast.Expr
expr rbp =
  lookchainl1
    (lookahead >>= \l -> nud l rbp)
    (\op -> precedence op > rbp)
    (lookahead >>= led)

led :: L.Lexeme -> Parser (Ast.Expr -> Ast.Expr)
led lexeme = case lexeme of
  L.Assign -> infixr' L.Assign Ast.Assign
  L.Or -> infixl' L.Or (`Ast.Binop` Ast.Or)
  L.And -> infixl' L.And (`Ast.Binop` Ast.And)
  L.Bar -> infixl' L.Bar (`Ast.Binop` Ast.BitwiseOr)
  L.Caret -> infixl' L.Caret (`Ast.Binop` Ast.BitwiseXor)
  L.Ampers -> infixl' L.Ampers (`Ast.Binop` Ast.BitwiseAnd)
  L.Equal -> infixl' L.Equal (`Ast.Binop` Ast.Equal)
  L.Neq -> infixl' L.Neq (`Ast.Binop` Ast.Neq)
  L.Less -> infixl' L.Less (`Ast.Binop` Ast.Less)
  L.Leq -> infixl' L.Leq (`Ast.Binop` Ast.Leq)
  L.Greater -> infixl' L.Greater (`Ast.Binop` Ast.Greater)
  L.Geq -> infixl' L.Geq (`Ast.Binop` Ast.Geq)
  L.Plus -> infixl' L.Plus (`Ast.Binop` Ast.Add)
  L.Minus -> infixl' L.Minus (`Ast.Binop` Ast.Sub)
  L.Asterisk -> infixl' L.Asterisk (`Ast.Binop` Ast.Mul)
  L.Div -> infixl' L.Div (`Ast.Binop` Ast.Div)
  L.Mod -> infixl' L.Mod (`Ast.Binop` Ast.Mod)
  L.LBrack -> arrayAccess
  L.Dot -> fieldAccess
  L.Arrow -> indirect
  _ -> expect isInfix >> return (const Ast.ExprError)

ctd :: L.Lexeme -> Parser Ast.Construct
ctd lexeme
  | L.isType lexeme = do
    typ <- type'
    nameId <- expect L.isIdent

    if isPrimitive typ
      then do
        funcDefOrFuncDeclOrVarDecl typ (name nameId)
      else do
        if (not . isPointer) typ
          then do
            structDecl (name nameId)
          else do
            globalVarDecl typ (name nameId)
  | otherwise = expect L.isType >> return Ast.ConstructError
  where
    name (L.Ident x) = x

std :: L.Lexeme -> Parser Ast.Statement
std lexeme
  | startsExpr lexeme = Ast.Expr <$> expr 0
  | L.isType lexeme = localVarDecl
  | lexeme == L.LBrace = Ast.BlockStatement <$> block
  | lexeme == L.While = while'
  | lexeme == L.For = for'
  | lexeme == L.If = if'
  | lexeme == L.Return = return'
  | otherwise = expect startsStmt >> return Ast.StatementError

nud :: L.Lexeme -> Int -> Parser Ast.Expr
nud lexeme rbp = case lexeme of
  (L.LitInt x) -> advance >> return (Ast.LitInt x)
  (L.LitDouble x) -> advance >> return (Ast.LitDouble x)
  (L.LitChar x) -> advance >> return (Ast.LitChar x)
  (L.LitString x) -> advance >> return (Ast.LitString x)
  (L.Ident x) -> identOrFuncCall
  L.LitNull -> advance >> return Ast.Null
  L.Ampers -> advance >> Ast.AddressOf <$> expr rbp
  L.Asterisk -> advance >> Ast.Deref <$> expr rbp
  L.Minus -> advance >> Ast.Negative <$> expr rbp
  L.Not -> advance >> Ast.Negate <$> expr rbp
  L.Sizeof -> sizeof
  L.LParen ->
    advance
      >> (>?)
        [ (startsType, typecast),
          (startsExpr, nested)
        ]
  _ -> expect startsExpr >> return Ast.ExprError

sizeof :: Parser Ast.Expr
sizeof = do
  expect (L.is L.Sizeof)
  expect (L.is L.LParen)
  expr <-
    (>?)
      [ (L.isType, Ast.Sizeof . Left <$> type'),
        (const True, Ast.Sizeof . Right <$> expr 0)
      ]
  expect (L.is L.RParen)
  return expr

identOrFuncCall :: Parser Ast.Expr
identOrFuncCall = do
  nameId <- expect L.isIdent
  (>?)
    [ (L.is L.LParen, funcCall (name nameId)),
      (const True, return (Ast.Ident (name nameId)))
    ]
  where
    name (L.Ident x) = x

funcCall :: String -> Parser Ast.Expr
funcCall func = do
  expect (L.is L.LParen)
  args <- actuals
  expect (L.is L.RParen)
  return (Ast.Call func args)

arrayAccess :: Parser (Ast.Expr -> Ast.Expr)
arrayAccess = do
  expect (L.is L.LBrack)
  index <- expr 0
  expect (L.is L.RBrack)
  return (`Ast.ArrayAccess` index)

fieldAccess :: Parser (Ast.Expr -> Ast.Expr)
fieldAccess = do
  expect (L.is L.Dot)
  field <- expect L.isIdent
  return (\exp -> Ast.FieldAccess exp (fieldName field))
  where
    fieldName (L.Ident name) = name

indirect :: Parser (Ast.Expr -> Ast.Expr)
indirect = do
  expect (L.is L.Arrow)
  field <- expect L.isIdent
  return (\exp -> Ast.Indirect exp (fieldName field))
  where
    fieldName (L.Ident name) = name

type' :: Parser Ast.Type
type' =
  (>?)
    [ (L.isType, primitiveType),
      (L.is L.Struct, structType)
    ]

primitiveType :: Parser Ast.Type
primitiveType = do
  typeTok <- expect L.isType
  Ast.PrimitiveType (getType typeTok) <$> stars
  where
    getType (L.Type typ) = typ

structType :: Parser Ast.Type
structType = do
  expect (L.is L.Struct)
  id <- expect L.isIdent
  Ast.StructType (structName id) <$> stars
  where
    structName (L.Ident x) = x

stars :: Parser Int
stars = do
  xs <- many (expect $ L.is L.Asterisk) (L.is L.Asterisk)
  return (length xs)

arraySizes :: Parser [Int]
arraySizes = many arraySize (L.is L.LBrack)

arraySize :: Parser Int
arraySize = do
  expect (L.is L.LBrack)
  size <- expect L.isLitInt
  expect (L.is L.RBrack)
  return (sizeOf size)
  where
    sizeOf (L.LitInt x) = x

formals :: Parser [Ast.Formal]
formals = sepBy formal' L.isType L.Comma
  where
    formal' = withFollowsRecovery formal (L.any [L.Comma, L.RParen]) Ast.FormalError

formal :: Parser Ast.Formal
formal = do
  typ <- type'
  ident <- expect L.isIdent
  return (Ast.Formal typ (formalName ident))
  where
    formalName (L.Ident x) = x

actuals :: Parser [Ast.Expr]
actuals = sepBy (expr 0) startsExpr L.Comma

nested :: Parser Ast.Expr
nested = do
  exp <- expr 0
  expect (L.is L.RParen)
  return (Ast.Nested exp)

typecast :: Parser Ast.Expr
typecast = do
  typ <- type'
  expect (L.is L.RParen)
  Ast.Typecast typ <$> expr 0

includes :: Parser [Ast.Directive]
includes = many include (L.is L.Include)

include :: Parser Ast.Directive
include = do
  expect (L.is L.Include)
  fileName <- expect L.isLitString
  return (Ast.Include (file fileName))
  where
    file (L.LitString name) = name

infixl' :: L.Lexeme -> (Ast.Expr -> Ast.Expr -> Ast.Expr) -> Parser (Ast.Expr -> Ast.Expr)
infixl' op lift = do
  expect (L.is op)
  right <- expr (precedence op)
  return (`lift` right)

infixr' :: L.Lexeme -> (Ast.Expr -> Ast.Expr -> Ast.Expr) -> Parser (Ast.Expr -> Ast.Expr)
infixr' op lift = do
  expect (L.is op)
  right <- expr (precedence op - 1)
  return (`lift` right)

parse :: String -> [T.Token] -> Either (ParseErrorBundle TokenStream Void) Ast.Program
parse file tokens = case result of
  (Left error) -> Left (bundle $ reverse (error : errors state))
  (Right ast) -> case errors state of
    [] -> Right ast
    errors -> Left (bundle $ reverse errors)
  where
    (result, state) = runState (runExceptT program) (ParserState tokens 0 [])
    bundle errors = ParseErrorBundle (Ne.fromList (nub errors)) initPosState
    initPosState =
      PosState
        { pstateInput =
            TokenStream
              { tokenStreamInput = intercalate ", " . map (L.display . T.lexeme) $ tokens,
                unTokenStream = tokens
              },
          pstateOffset = 0,
          pstateSourcePos = initialPos file,
          pstateTabWidth = mkPos 8,
          pstateLinePrefix = "" -- fix this
        }

parse' file input = do
  isPretty <- supportsPretty
  case lex' file input of
    (Right tokens) -> case parse file tokens of
      (Right ast) -> putStrLn $ visualiseAst ast
      (Left bundle) -> do
        print bundle
        putStrLn $ prettyPrintErrors bundle (pack input) isPretty
    (Left bundle) -> putStrLn $ prettyPrintErrors bundle (pack input) isPretty