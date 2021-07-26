module Semant.Type where

import qualified Parser.Ast as Ast

data Type = Scalar Ast.Type | Array Ast.Type [Int] | Any deriving (Show, Eq)