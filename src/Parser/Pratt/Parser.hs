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
import qualified Parser.Ast as Ast
import Parser.AstVisualiser (visualiseAst)
import Parser.Combinator.TokenStream (TokenStream (..))
import Parser.Errors.PrettyPrinter (prettyPrintErrors)
import Parser.Grammar.Firsts (startsExpr)
import Parser.Grammar.Follows (followsExp, followsStatement)
import Parser.Pratt.Combinators.Chains (lookchainl1)
import Parser.Pratt.Combinators.Prim (many, sepBy)
import Parser.Pratt.Combinators.Recovery (withFollowsRecovery)
import Parser.Pratt.Prim (Parser, ParserError, ParserState (ParserState, errors), advance, expect, head, liftToken, lookahead, unexpected, unexpectedEof)
import System.Console.Pretty (supportsPretty)
import Text.Megaparsec (MonadParsec (lookAhead), ParseErrorBundle (ParseErrorBundle), PosState (PosState, pstateInput, pstateLinePrefix, pstateOffset, pstateSourcePos, pstateTabWidth), initialPos, mkPos, pos1)
import Prelude hiding (head)

program :: Parser Ast.Program
program = do
  expect (L.is L.LParen)
  expect (L.is L.Struct)
  return (Ast.Program [] [])

block :: Parser Ast.Block
block = do
  expect (L.is L.LBrace)
  stmts <- many statement' startsExpr -- fix starts expr with starts stmt
  expect (L.is L.RBrace)
  return (Ast.Block stmts)
  where
    statement' = withFollowsRecovery statement followsStatement Ast.StatementError

statement :: Parser Ast.Statement
statement = do
  exp <- withFollowsRecovery (expr 0) followsExp Ast.ExprError
  expect (L.is L.Semi)
  return (Ast.Expr exp)

expr :: Int -> Parser Ast.Expr
expr rbp =
  lookchainl1
    (head >>= \tok -> nud tok)
    (\op -> precedence op > rbp)
    (head >>= \tok -> led tok)

led :: T.Token -> Parser (Ast.Expr -> Ast.Expr)
led T.Token {T.lexeme} = case lexeme of
  L.Assign -> infixr' L.Assign Ast.Assign
  L.Or -> infixl' L.Or (`Ast.Binop` Ast.Or)
  L.And -> infixl' L.And (`Ast.Binop` Ast.And)
  L.Bar -> infixl' L.Bar (`Ast.Binop` Ast.BitwiseOr)
  L.Caret -> infixl' L.Caret (`Ast.Binop` Ast.BitwiseXor)
  L.Ampers -> infixl' L.Ampers (`Ast.Binop` Ast.BitwiseAnd)
  L.Equal -> infixl' L.Equal (`Ast.Binop` Ast.Equal)
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
  _ -> undefined

nud :: T.Token -> Parser Ast.Expr
nud T.Token {T.lexeme} = case lexeme of
  (L.LitInt x) -> advance >> return (Ast.LitInt x)
  (L.LitDouble x) -> advance >> return (Ast.LitDouble x)
  (L.LitChar x) -> advance >> return (Ast.LitChar x)
  (L.LitString x) -> advance >> return (Ast.LitString x)
  (L.Ident x) -> identOrFuncCall
  L.LitNull -> advance >> return Ast.Null
  L.LParen ->
    lookahead >>= \lexeme ->
      if L.isType lexeme
        then do typecast
        else do expr 0
  _ ->
    expect
      ( \lexeme ->
          L.any
            [ L.LParen,
              L.Minus,
              L.Not,
              L.Asterisk,
              L.Ampers,
              L.Sizeof
            ]
            lexeme
            || L.isLit lexeme
            || L.isIdent lexeme
      )
      >> return Ast.ExprError

identOrFuncCall :: Parser Ast.Expr
identOrFuncCall = do
  nameId <- expect L.isIdent
  lookahead >>= \case
    L.LParen -> funcCall (name nameId)
    _ -> return (Ast.Ident (name nameId))
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

actuals :: Parser [Ast.Expr]
actuals = sepBy (expr 0) startsExpr L.Comma

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

nested :: Parser Ast.Expr
nested = do
  expect (L.is L.LParen)
  exp <- expr 0
  expect (L.is L.RParen)
  return (Ast.Nested exp)

typecast :: Parser Ast.Expr
typecast = undefined

precedence :: L.Lexeme -> Int
precedence L.Assign = 10
precedence L.Or = 20
precedence L.And = 30
precedence L.Bar = 40
precedence L.Caret = 50
precedence L.Ampers = 60
precedence L.Equal = 70
precedence L.Less = 80
precedence L.Leq = 80
precedence L.Greater = 80
precedence L.Geq = 90
precedence L.Plus = 100
precedence L.Minus = 100
precedence L.Asterisk = 110
precedence L.Div = 110
precedence L.Mod = 110
precedence L.Dot = 120
precedence L.LBrack = 120
precedence L.Arrow = 120
precedence _ = 0

parse file tokens = case result of
  (Left error) -> Left (bundle $ reverse (error : errors state))
  (Right ast) -> case errors state of
    [] -> Right ast
    errors -> Left (bundle $ reverse errors)
  where
    (result, state) = runState (runExceptT block) (ParserState tokens 0 [])
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