module Semant.Builtins where

import Lexer.Lexeme (BuiltinType (Int, Void, Char))
import qualified Parser.Ast as Ast
import Semant.Ast.SemantAst
import Semant.Type
import Parser.Ast

builtins :: [SFunction]
builtins =
  [ SFunction
      { returnType = voidTyp,
        funcName = "printf",
        formals = [SFormal (Scalar (Ast.PrimitiveType Char 1)) "formatStr"],
        body = Just (SBlock [])
      },
    SFunction
      { returnType = voidTyp,
        funcName = "scanf",
        formals = [SFormal (Scalar (Ast.PrimitiveType Char 1)) "formatStr"],
        body = Just (SBlock [])
      },
    SFunction
      { returnType = Scalar (Ast.PrimitiveType Void 1),
        funcName = "malloc",
        formals = [SFormal (Scalar (Ast.PrimitiveType Int 0)) "size"],
        body = Just (SBlock [])
      },
    SFunction
      { returnType = voidTyp,
        funcName = "free",
        formals = [SFormal (Scalar (Ast.PrimitiveType Void 1)) "ptr"],
        body = Just (SBlock [])
      }
  ]

isBuiltin :: SFunction -> Bool 
isBuiltin func = func `elem` builtins