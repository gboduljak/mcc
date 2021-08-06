module Semant.Builtins where

import Lexer.Lexeme (BuiltinType (Int, Void))
import qualified Parser.Ast as Ast
import Semant.Ast.SemantAst
import Semant.Type

builtins :: [SFunction]
builtins =
  [ SFunction
      { returnType = voidTyp,
        funcName = "printf",
        formals = [],
        body = Just (SBlock [])
      },
    SFunction
      { returnType = voidTyp,
        funcName = "scanf",
        formals = [],
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