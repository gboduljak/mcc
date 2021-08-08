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
import Parser.Ast (getBlockOffset, getExprOff, isPointer, isPrimitive)
import qualified Parser.Ast as Ast
import Parser.AstVisualiser (visualiseAst)
import Parser.Combinator.TokenStream (TokenStream (..))
import Parser.Errors.Merger (mergeErrorsBasedOnPos)
import Parser.Errors.PrettyPrinter (prettyPrintErrors)
import Parser.Grammar.Firsts (startsExpr, startsStmt, startsType)
import Parser.Grammar.Follows (followsConstruct, followsExp, followsStatement)
import Parser.Grammar.Operators (isInfix, precedence, typecastPrecedence, unopPrecedence)
import Parser.Pratt.Combinators.Chains (lookchainl1)
import Parser.Pratt.Combinators.Prim (many, many1, sepBy, sepBy1)
import Parser.Pratt.Combinators.Recovery (withFollowsRecovery)
import Parser.Pratt.Combinators.When ((>?))
import Parser.Pratt.Prim
import System.Console.Pretty (supportsPretty)
import Text.Megaparsec
  ( MonadParsec (lookAhead),
    ParseErrorBundle (ParseErrorBundle),
    PosState (PosState, pstateInput, pstateLinePrefix, pstateOffset, pstateSourcePos, pstateTabWidth),
    initialPos,
    mkPos,
    pos1,
  )
import Prelude hiding (head)

program :: Parser Ast.Program
program = do
  dirs <- directives
  constrs <- constructs
  expect (L.is L.Eof)
  return (Ast.Program dirs constrs)

directives :: Parser [Ast.Directive]
directives = includes

constructs :: Parser [Ast.Construct]
constructs = many construct startsType

construct :: Parser Ast.Construct
construct = do
  off <- getOffset
  withFollowsRecovery
    (lookahead >>= ctd)
    followsConstruct
    (return Ast.ConstructError)

structDecl :: String -> Parser Ast.Construct
structDecl structName = do
  off <- getOffset
  vars <- structFields
  expect (L.is L.Semi)
  return (Ast.StructDecl (Ast.Struct structName vars off))

structFields :: Parser [Ast.VarDecl]
structFields = do
  expect (L.is L.LBrace)
  fields <- many1 structField startsType
  expect (L.is L.RBrace)
  return fields

structField :: Parser Ast.VarDecl
structField =
  withFollowsRecovery
    ( do
        off <- getOffset
        typ <- type'
        nameId <- expect L.isIdent
        field <- varDecl off typ (name nameId)
        expect (L.is L.Semi)
        return field
    )
    (\lexeme -> L.any [L.RBrace] lexeme || startsType lexeme)
    (do Ast.VarDeclError <$> getOffset)
  where
    name (L.Ident x) = x

funcDefOrFuncDeclOrVarDecl :: Int -> Ast.Type -> String -> Parser Ast.Construct
funcDefOrFuncDeclOrVarDecl off typ name = do
  (>?)
    [ (L.is L.LParen, func' typ name),
      (L.is L.LBrack, globalVarDecl off typ name),
      (L.is L.Semi, globalVarDecl off typ name)
    ]
  where
    func' typ name = do
      args <- do
        expect (L.is L.LParen)
        args <- formals
        expect (L.is L.RParen)
        return args
      (>?)
        [ (L.is L.Semi, Ast.FuncDecl <$> funcDecl off typ name args),
          (L.is L.LBrace, Ast.FuncDefn <$> funcDefn off typ name args)
        ]

globalVarDecl :: Int -> Ast.Type -> String -> Parser Ast.Construct
globalVarDecl off typ name = do
  decl <- Ast.VarDecl <$> varDecl off typ name
  expect (L.is L.Semi)
  return decl

funcDecl :: Int -> Ast.Type -> String -> [Ast.Formal] -> Parser Ast.FuncDecl
funcDecl off rettyp funcName formals = do
  expect (L.is L.Semi)
  return (Ast.Func rettyp funcName formals off)

funcDefn :: Int -> Ast.Type -> String -> [Ast.Formal] -> Parser Ast.FuncDef
funcDefn off rettyp funcName formals = do
  body <- block
  return (Ast.FuncDef rettyp funcName formals body off)

statement :: Parser Ast.Statement
statement = do
  off <- getOffset
  withFollowsRecovery
    ( do
        stmt <- lookahead >>= std
        if followedBySemi stmt
          then expect (L.is L.Semi) >> return stmt
          else return stmt
    )
    followsStatement
    (return (Ast.StatementError off))
  where
    followedBySemi Ast.While {} = False
    followedBySemi Ast.For {} = False
    followedBySemi Ast.If {} = False
    followedBySemi Ast.BlockStatement {} = False
    followedBySemi _ = True

block :: Parser Ast.Block
block = do
  off <- getOffset
  expect (L.is L.LBrace)
  stmts <- many statement startsStmt
  expect (L.is L.RBrace)
  return (Ast.Block stmts off)

localVarDecl :: Parser Ast.Statement
localVarDecl = do
  off <- getOffset
  typ <- type'
  nameId <- expect L.isIdent
  decl <- varDecl off typ (name nameId)
  return (Ast.VarDeclStatement decl off)
  where
    name (L.Ident x) = x

varDecl :: Int -> Ast.Type -> String -> Parser Ast.VarDecl
varDecl off typ name = do sizes <- arraySizes; return (Ast.Var typ name sizes off)

while' :: Parser Ast.Statement
while' = do
  off <- getOffset
  expect (L.is L.While)
  expect (L.is L.LParen)
  cond <- expr 0
  expect (L.is L.RParen)
  body <- statement
  return (Ast.While cond body off)

for' :: Parser Ast.Statement
for' = do
  off <- getOffset
  expect (L.is L.For)
  expect (L.is L.LParen)
  init <- optionalExpr
  expect (L.is L.Semi)
  cond <- optionalExpr
  expect (L.is L.Semi)
  incr <- optionalExpr
  expect (L.is L.RParen)
  body <- statement
  return (Ast.For init cond incr body off)

if' :: Parser Ast.Statement
if' = do
  off <- getOffset
  expect (L.is L.If)
  expect (L.is L.LParen)
  cond <- expr 0
  expect (L.is L.RParen)
  conseq <- statement
  alt' <- alt
  return (Ast.If cond conseq alt' off)
  where
    alt =
      (>?)
        [ (L.is L.Else, expect (L.is L.Else) >> Just <$> statement),
          (const True, return Nothing)
        ]

return' :: Parser Ast.Statement
return' = do
  off <- getOffset
  expect (L.is L.Return)
  expr <- optionalExpr
  return (Ast.Return expr off)

optionalExpr :: Parser (Maybe Ast.Expr)
optionalExpr =
  (>?)
    [ (startsExpr, Just <$> expr 0),
      (const True, return Nothing)
    ]

expr :: Int -> Parser Ast.Expr
expr rbp = do
  off <- getOffset
  withFollowsRecovery
    ( lookchainl1
        (lookahead >>= \lexeme -> nud lexeme rbp)
        (\op -> precedence op > rbp)
        (lookahead >>= led)
    )
    followsExp
    (return (Ast.ExprError off))

ctd :: L.Lexeme -> Parser Ast.Construct
ctd lexeme
  | startsType lexeme = do
    off <- getOffset
    typ <- type'
    if isPrimitive typ || isPointer typ
      then do
        name <- nameFrom <$> expect L.isIdent
        funcDefOrFuncDeclOrVarDecl off typ name
      else do
        (>?)
          [ (L.is L.LBrace, structDecl (structName typ)),
            (L.isIdent, expect L.isIdent >>= funcDefOrFuncDeclOrVarDecl off typ . nameFrom)
          ]
  | otherwise = expect startsType >> return Ast.ConstructError
  where
    nameFrom (L.Ident x) = x
    structName (Ast.StructType x _) = x

std :: L.Lexeme -> Parser Ast.Statement
std lexeme
  | startsExpr lexeme = do exp <- expr 0; return (Ast.Expr exp (getExprOff exp))
  | startsType lexeme = localVarDecl
  | lexeme == L.LBrace = do blk <- block; return (Ast.BlockStatement blk (getBlockOffset blk))
  | lexeme == L.While = while'
  | lexeme == L.For = for'
  | lexeme == L.If = if'
  | lexeme == L.Return = return'
  | otherwise = do
    off <- getOffset
    expect startsStmt
    return (Ast.StatementError off)

nud :: L.Lexeme -> Int -> Parser Ast.Expr
nud lexeme rbp = do
  off <- getOffset
  case lexeme of
    (L.LitInt x) -> advance >> return (Ast.LitInt x off)
    (L.LitDouble x) -> advance >> return (Ast.LitDouble x off)
    (L.LitChar x) -> advance >> return (Ast.LitChar x off)
    (L.LitString x) -> advance >> return (Ast.LitString x off)
    (L.Ident x) -> identOrFuncCall
    L.LitNull -> advance >> return (Ast.Null off)
    L.Ampers -> advance >> do expr <- expr unopPrecedence; return (Ast.AddressOf expr off)
    L.Asterisk -> advance >> do expr <- expr unopPrecedence; return (Ast.Deref expr off)
    L.Minus -> advance >> do expr <- expr unopPrecedence; return (Ast.Negative expr off)
    L.Not -> advance >> do expr <- expr unopPrecedence; return (Ast.Negate expr off)
    L.Sizeof -> sizeof
    L.LParen ->
      advance
        >> (>?)
          [ (startsType, typecast rbp),
            (startsExpr, nested)
          ]
    _ -> expect startsExpr >> return (Ast.ExprError off)

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
  _ -> do
    expect isInfix
    const . Ast.ExprError <$> getOffset

infixl' :: L.Lexeme -> (Ast.Expr -> Ast.Expr -> Int -> Ast.Expr) -> Parser (Ast.Expr -> Ast.Expr)
infixl' op lift = do
  expect (L.is op)
  right <- expr (precedence op)
  return (\left -> lift left right (getExprOff left))

infixr' :: L.Lexeme -> (Ast.Expr -> Ast.Expr -> Int -> Ast.Expr) -> Parser (Ast.Expr -> Ast.Expr)
infixr' op lift = do
  expect (L.is op)
  right <- expr (precedence op - 1)
  return (\left -> lift left right (getExprOff left))

sizeof :: Parser Ast.Expr
sizeof = do
  off <- getOffset
  expect (L.is L.Sizeof)
  expect (L.is L.LParen)
  expr <-
    (>?)
      [ (startsType, do typ <- sizeofType; return (Ast.Sizeof (Left typ) off)),
        (const True, do exp <- expr 0; return (Ast.Sizeof (Right exp) off))
      ]
  expect (L.is L.RParen)
  return expr

sizeofType :: Parser Ast.SizeofType
sizeofType = do
  typ <- type'
  Ast.SizeofType typ <$> arraySizes

identOrFuncCall :: Parser Ast.Expr
identOrFuncCall = do
  off <- getOffset
  nameId <- expect L.isIdent
  (>?)
    [ (L.is L.LParen, funcCall (name nameId)),
      (const True, return (Ast.Ident (name nameId) off))
    ]
  where
    name (L.Ident x) = x

funcCall :: String -> Parser Ast.Expr
funcCall func = do
  off <- getOffset
  expect (L.is L.LParen)
  args <- actuals
  expect (L.is L.RParen)
  return (Ast.Call func args (off - 1))

arrayAccess :: Parser (Ast.Expr -> Ast.Expr)
arrayAccess = do
  off <- getOffset
  expect (L.is L.LBrack)
  index <- expr 0
  expect (L.is L.RBrack)
  return (\target -> Ast.ArrayAccess target index (off -1))

fieldAccess :: Parser (Ast.Expr -> Ast.Expr)
fieldAccess = do
  off <- getOffset
  expect (L.is L.Dot)
  field <- expect L.isIdent
  return (\exp -> Ast.FieldAccess exp (fieldName field) (off - 1))
  where
    fieldName (L.Ident name) = name

indirect :: Parser (Ast.Expr -> Ast.Expr)
indirect = do
  off <- getOffset
  expect (L.is L.Arrow)
  field <- expect L.isIdent
  return (\exp -> Ast.Indirect exp (fieldName field) (off - 1))
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
formals = sepBy formal' startsType L.Comma
  where
    formal' = do
      off <- getOffset
      withFollowsRecovery formal (L.any [L.Comma, L.RParen]) (return (Ast.FormalError off))

formal :: Parser Ast.Formal
formal = do
  off <- getOffset
  typ <- type'
  ident <- expect L.isIdent
  return (Ast.Formal typ (formalName ident) off)
  where
    formalName (L.Ident x) = x

actuals :: Parser [Ast.Expr]
actuals = sepBy (expr 0) startsExpr L.Comma

nested :: Parser Ast.Expr
nested = do
  off <- getOffset
  exp <- expr 0
  expect (L.is L.RParen)
  return (Ast.Nested exp off)

typecast :: Int -> Parser Ast.Expr
typecast rbp = do
  off <- getOffset
  typ <- type'
  expect (L.is L.RParen)
  exp <- expr typecastPrecedence
  return (Ast.Typecast typ exp off)

includes :: Parser [Ast.Directive]
includes = many include (L.is L.Include)

include :: Parser Ast.Directive
include = do
  expect (L.is L.Include)
  fileName <- expect L.isLitString
  return (Ast.Include (file fileName))
  where
    file (L.LitString name) = name

parseExprs :: String -> [T.Token] -> Either (ParseErrorBundle TokenStream Void) [Ast.Expr]
parseExprs file tokens = case result of
  (Left error) -> Left (bundle $ reverse (error : errors state))
  (Right ast) -> case errors state of
    [] -> Right ast
    errors -> Left (bundle $ reverse errors)
  where
    (result, state) = runState (runExceptT (sepBy (expr 0) startsExpr L.Semi)) (ParserState tokens 0 [])
    bundle errors = ParseErrorBundle (Ne.fromList (nub . mergeErrorsBasedOnPos $ errors)) initPosState
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
          pstateLinePrefix = ""
        }

parseStatements :: String -> [T.Token] -> Either (ParseErrorBundle TokenStream Void) [Ast.Statement]
parseStatements file tokens = case result of
  (Left error) -> Left (bundle $ reverse (error : errors state))
  (Right ast) -> case errors state of
    [] -> Right ast
    errors -> Left (bundle $ reverse errors)
  where
    (result, state) = runState (runExceptT (many statement startsStmt)) (ParserState tokens 0 [])
    bundle errors = ParseErrorBundle (Ne.fromList (nub . mergeErrorsBasedOnPos $ errors)) initPosState
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
          pstateLinePrefix = ""
        }

parseExpr :: String -> [T.Token] -> Either (ParseErrorBundle TokenStream Void) Ast.Expr
parseExpr file tokens = case result of
  (Left error) -> Left (bundle $ reverse (error : errors state))
  (Right ast) -> case errors state of
    [] -> Right ast
    errors -> Left (bundle $ reverse errors)
  where
    (result, state) = runState (runExceptT (expr 0)) (ParserState tokens 0 [])
    bundle errors = ParseErrorBundle (Ne.fromList (nub . mergeErrorsBasedOnPos $ errors)) initPosState
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
          pstateLinePrefix = ""
        }

parse :: String -> [T.Token] -> Either (ParseErrorBundle TokenStream Void) Ast.Program
parse file tokens = case result of
  (Left error) -> Left (bundle $ reverse (error : errors state))
  (Right ast) -> case errors state of
    [] -> Right ast
    errors -> Left (bundle $ reverse errors)
  where
    (result, state) = runState (runExceptT program) (ParserState tokens 0 [])
    bundle errors = ParseErrorBundle (Ne.fromList (nub . mergeErrorsBasedOnPos $ errors)) initPosState
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
          pstateLinePrefix = ""
        }

-- For debugging
-- parse' file input = do
--   isPretty <- supportsPretty
--   case lex' file input of
--     (Right tokens) -> case parse file tokens of
--       (Right ast) -> putStrLn $ visualiseAst ast
--       (Left bundle) -> do
--         print bundle
--         putStrLn $ prettyPrintErrors bundle (pack input) isPretty
--     (Left bundle) -> putStrLn $ prettyPrintErrors bundle (pack input) isPretty