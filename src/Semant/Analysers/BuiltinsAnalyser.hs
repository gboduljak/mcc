{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}

module Semant.Analysers.BuiltinsAnalyser where

import Control.Monad
import Lexer.Lexeme (BuiltinType (Char, Double, Int, Void))
import Parser.Ast as Ast hiding (Type)
import Semant.Analysers.CallArgAnalyser
import Semant.Ast.SemantAst
import Semant.Errors.SemantError hiding (Void)
import Semant.Semant
import Semant.Type

-- analyseScanf :: String -> [SExpr] -> Expr -> Semant SExpr
-- analyseScanf formatStr = analyseStringIo "scanf"
data StringIoFunc = Printf | Scanf

analysePrintf :: String -> [SExpr] -> Expr -> Semant SExpr
analysePrintf = analyseStringIo Printf

analyseScanf :: String -> [SExpr] -> Expr -> Semant SExpr
analyseScanf = analyseStringIo Scanf

analyseStringIo :: StringIoFunc -> String -> [SExpr] -> Expr -> Semant SExpr
analyseStringIo func formatStr formatArgs expr = do
  if length actuals == length expectedFormals
    then do
      !validArgs <- zipWithM (analyseArgBind funcName expr) expectedFormals actuals
      if and validArgs
        then do return (voidTyp, SCall funcName actuals)
        else do return (Any, SCall funcName actuals)
    else do
      registerError (CallArgsNumberError funcName (length expectedFormals) (length actuals) expr)
      return (Any, SCall funcName actuals)
  where
    actuals = (Scalar (PrimitiveType Char 1), SLitString formatStr) : formatArgs
    funcName = case func of
      Printf -> "printf"
      Scanf -> "scanf"
    formatArgTypes = parseFormatString formatStr func
    formatStringArg = SFormal (Scalar (Ast.PrimitiveType Char 1)) "formatString"
    expectedFormals = formatStringArg : zipWith (\i typ -> SFormal typ ("arg" ++ show i)) [0 ..] formatArgTypes

parseFormatString :: String -> StringIoFunc -> [Type]
parseFormatString "" Printf = []
parseFormatString ('%' : x : xs) Printf
  | x == 's' = Scalar (Ast.PrimitiveType Char 1) : parseFormatString xs Printf
  | x == 'c' = Scalar (Ast.PrimitiveType Char 0) : parseFormatString xs Printf
  | x == 'd' = Scalar (Ast.PrimitiveType Int 0) : parseFormatString xs Printf
  | x == 'f' = Scalar (Ast.PrimitiveType Double 0) : parseFormatString xs Printf
  | otherwise = parseFormatString (x : xs) Printf
parseFormatString (x : xs) Printf = parseFormatString xs Printf
parseFormatString "" Scanf = []
parseFormatString ('%' : x : xs) Scanf
  | x == 's' = Scalar (Ast.PrimitiveType Char 1) : parseFormatString xs Scanf
  | x == 'c' = Scalar (Ast.PrimitiveType Char 1) : parseFormatString xs Scanf
  | x == 'd' = Scalar (Ast.PrimitiveType Int 1) : parseFormatString xs Scanf
  | x == 'f' = Scalar (Ast.PrimitiveType Double 1) : parseFormatString xs Scanf
  | otherwise = parseFormatString (x : xs) Scanf
parseFormatString (x : xs) Scanf = parseFormatString xs Scanf