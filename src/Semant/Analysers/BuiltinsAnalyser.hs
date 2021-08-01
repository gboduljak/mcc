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

analysePrintf :: String -> [SExpr] -> Expr -> Semant SExpr
analysePrintf formatStr formatArgs expr =
  if length actuals == length expectedFormals
    then do
      !validArgs <- zipWithM (analyseArgBind "printf" expr) expectedFormals actuals
      if and validArgs
        then do return (voidTyp, SCall "printf" actuals)
        else do return (Any, SCall "printf" actuals)
    else do
      registerError (CallArgsNumberError "printf" (length expectedFormals) (length actuals) expr)
      return (Any, SCall "printf" actuals)
  where
    formatArgTypes = parseFormatString formatStr
    formatStringArg = SFormal (Scalar (Ast.PrimitiveType Char 1)) "formatString"
    expectedFormals = formatStringArg : zipWith (\i typ -> SFormal typ ("arg" ++ show i)) [0 ..] formatArgTypes
    actuals = (Scalar (PrimitiveType Char 1), SLitString formatStr) : formatArgs

parseFormatString :: String -> [Type]
parseFormatString "" = []
parseFormatString ('%' : x : xs)
  | x == 's' = Scalar (Ast.PrimitiveType Char 1) : parseFormatString xs
  | x == 'c' = Scalar (Ast.PrimitiveType Char 0) : parseFormatString xs
  | x == 'd' = Scalar (Ast.PrimitiveType Int 0) : parseFormatString xs
  | x == 'f' = Scalar (Ast.PrimitiveType Double 0) : parseFormatString xs
  | otherwise = parseFormatString (x : xs)
parseFormatString (x : xs) = parseFormatString xs