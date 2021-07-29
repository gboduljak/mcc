{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Semant.Ast.SemantAstVisualiser where

import Control.Monad.State (MonadState (get), State, gets, modify, runState)
import Data.Foldable (traverse_)
import Data.List
import qualified Lexer.Lexeme as L
import Parser.Ast (InfixOp (..), Type (PrimitiveType, StructType))
import qualified Parser.Ast as Ast
import Semant.Ast.SemantAst
import Semant.Type
import Prelude hiding (id)

class SemantAstDrawable a where
  visualise :: SemantAstDrawable () => a -> SemantAstVisualiser Int

data SemantAstVisualiserState = SemantAstVisualiserState Int [String]

type SemantAstVisualiser a = SemantAstDrawable a => State SemantAstVisualiserState a

id :: SemantAstVisualiserState -> Int
id (SemantAstVisualiserState nodeId output) = nodeId

output :: SemantAstVisualiserState -> [String]
output (SemantAstVisualiserState nodeId output) = output

connect :: Int -> Int -> SemantAstVisualiser ()
connect u v = emit ("    node" ++ show u ++ "->" ++ "node" ++ show v)

emit :: String -> SemantAstVisualiser ()
emit line =
  modify (\(SemantAstVisualiserState nodeId output) -> SemantAstVisualiserState nodeId (line : output))

emitNode :: Int -> String -> SemantAstVisualiser ()
emitNode id label = emit ("    node" ++ show id ++ "[label=\"" ++ label ++ "\"]")

nextId :: SemantAstVisualiser Int
nextId = do
  id <- gets id
  modify (\(SemantAstVisualiserState nodeId output) -> SemantAstVisualiserState (nodeId + 1) output)
  return id

reverseOutput :: SemantAstVisualiser ()
reverseOutput = modify (\(SemantAstVisualiserState nodeId output) -> SemantAstVisualiserState nodeId (reverse output))

visualiseSemantAst :: SemantAstDrawable a => a -> String
visualiseSemantAst ast = intercalate "\n" (output state)
  where
    (_, state) = runState visualise' (SemantAstVisualiserState 0 [])
    visualise' = do
      emit "digraph g {"
      visualise ast
      emit "}"
      reverseOutput

-- instance SemantAstDrawable SProgram where
--   visualise (SProgram includes constructs) = do
--     programId <- nextId
--     includesId <- nextId
--     emit
--       ( "    node"
--           ++ show programId
--           ++ "[label=<<B>Program</B>>"
--           ++ ", shape=record]"
--       )
--     emit
--       ( "    node"
--           ++ show includesId
--           ++ "[label=\"<f0> Includes"
--           ++ "\", shape=record]"
--       )
--     connect programId includesId
--     traverse_
--       ( \include -> do
--           includeId <- visualise include
--           connect includesId includeId
--       )
--       includes
--     traverse_
--       ( \constr -> do
--           constrId <- visualise constr
--           connect programId constrId
--       )
--       constructs
--     return programId

-- instance SemantAstDrawable Construct where
--   visualise (FuncDecl decl) = visualise decl
--   visualise (FuncDefn defn) = visualise defn
--   visualise (StructDecl decl) = visualise decl
--   visualise (VarDecl decl) = visualise decl

-- instance SemantAstDrawable Directive where
--   visualise (Include file) = do
--     includeId <- nextId
--     emit
--       ( "    node"
--           ++ show includeId
--           ++ "[label=\"<f0>"
--           ++ "#include"
--           ++ " | <f1> "
--           ++ escape file
--           ++ "\", shape=record]"
--       )
--     return includeId

-- instance SemantAstDrawable VarDecl where
--   visualise decl@(Var typ name arraySizes) = do
--     declNodeId <- nextId
--     nameId <- nextId
--     if isArray decl
--       then do
--         let sizes = concat ["[" ++ show size ++ "]" | size <- arraySizes]
--         emit
--           ( "    node"
--               ++ show declNodeId
--               ++ "[label=\"<f0> VarDecl | <f1>"
--               ++ display typ
--               ++ " | <f2> "
--               ++ escape name
--               ++ " | <f3> "
--               ++ sizes
--               ++ "\", shape=record]"
--           )
--         return declNodeId
--       else do
--         emit
--           ( "    node"
--               ++ show declNodeId
--               ++ "[label=\"<f0> VarDecl | <f1>"
--               ++ display typ
--               ++ " | <f2> "
--               ++ escape name
--               ++ "\", shape=record]"
--           )
--         return declNodeId

-- instance SemantAstDrawable StructDecl where
--   visualise decl@(Struct name vardecls) = do
--     declNodeId <- nextId
--     emit
--       ( "    node"
--           ++ show declNodeId
--           ++ "[label=\"<f0> StructDecl | <f1>"
--           ++ escape name
--           ++ " | <f2> "
--           ++ "fields"
--           ++ "\", shape=record]"
--       )
--     traverse_
--       ( \varDecl -> do
--           varDeclId <- visualise varDecl
--           emit ("    node" ++ show declNodeId ++ ":f2 -> node" ++ show varDeclId ++ ";")
--       )
--       vardecls

--     return declNodeId

-- instance SemantAstDrawable Formal where
--   visualise (Formal typ name) = do
--     formalId <- nextId
--     emit
--       ( "    node"
--           ++ show formalId
--           ++ "[label=\"<f0>"
--           ++ display typ
--           ++ " | <f1> "
--           ++ escape name
--           ++ "\", shape=record]"
--       )
--     return formalId

-- instance SemantAstDrawable FuncDef where
--   visualise decl@(FuncDef rettyp name formals body) = do
--     declNodeId <- nextId
--     emit
--       ( "    node"
--           ++ show declNodeId
--           ++ "[label=\"<f0>FuncDef"
--           ++ " | <f1>"
--           ++ display rettyp
--           ++ " | <f2> "
--           ++ escape name
--           ++ " | <f3> "
--           ++ "formals"
--           ++ " | <f4> "
--           ++ "body"
--           ++ "\", shape=record]"
--       )
--     traverse_
--       ( \formal -> do
--           formalId <- visualise formal
--           emit ("    node" ++ show declNodeId ++ ":f3 -> node" ++ show formalId)
--       )
--       formals

--     bodyId <- visualise body
--     emit ("    node" ++ show declNodeId ++ ":f4 -> node" ++ show bodyId)

--     return declNodeId

-- instance SemantAstDrawable FuncDecl where
--   visualise decl@(Func rettyp name formals) = do
--     declNodeId <- nextId
--     emit
--       ( "    node"
--           ++ show declNodeId
--           ++ "[label=\"<f0>FuncDecl"
--           ++ " | <f1>"
--           ++ display rettyp
--           ++ " | <f2> "
--           ++ escape name
--           ++ " | <f3> "
--           ++ "formals"
--           ++ "\", shape=record]"
--       )
--     traverse_
--       ( \formal -> do
--           formalId <- visualise formal
--           emit ("    node" ++ show declNodeId ++ ":f3 -> node" ++ show formalId)
--       )
--       formals

--     return declNodeId

-- instance SemantAstDrawable Block where
--   visualise (Block stmts) = do
--     blockId <- nextId
--     lbraceId <- nextId
--     rbraceId <- nextId
--     emit
--       ( "    node"
--           ++ show blockId
--           ++ "[label=\"<f0>"
--           ++ "Block"
--           ++ "\", shape=record]"
--       )
--     emitNode lbraceId "{"
--     connect blockId lbraceId
--     traverse_
--       ( \stmt -> do
--           stmtId <- visualise stmt
--           connect blockId stmtId
--       )
--       stmts
--     emitNode rbraceId "}"
--     connect blockId rbraceId
--     return blockId

instance SemantAstDrawable (Maybe SExpr) where
  visualise (Just expr) = visualise expr
  visualise Nothing = do
    emptyId <- nextId
    emitNode emptyId "{}"
    return emptyId

-- instance SemantAstDrawable Statement where
--   visualise (Expr expr) = visualise expr
--   visualise (BlockStatement stmt) = visualise stmt
--   visualise (VarDeclStatement stmt) = visualise stmt
--   visualise (While cond body) = do
--     whileId <- nextId
--     emit
--       ( "    node"
--           ++ show whileId
--           ++ "[label=\"<f0>While"
--           ++ " | <f1>"
--           ++ "cond"
--           ++ " | <f2> "
--           ++ "body"
--           ++ "\", shape=record]"
--       )
--     condId <- visualise cond
--     emit ("    node" ++ show whileId ++ ":f1 -> node" ++ show condId)
--     bodyId <- visualise body
--     emit ("    node" ++ show whileId ++ ":f2 -> node" ++ show bodyId)
--     return whileId
--   visualise (For init cond incr body) = do
--     forId <- nextId
--     emit
--       ( "    node"
--           ++ show forId
--           ++ "[label=\"<f0>For"
--           ++ " | <f1>"
--           ++ "init"
--           ++ " | <f2> "
--           ++ "cond"
--           ++ " | <f3> "
--           ++ "incr"
--           ++ " | <f4> "
--           ++ "body"
--           ++ "\", shape=record]"
--       )
--     initId <- visualise init
--     emit ("    node" ++ show forId ++ ":f1 -> node" ++ show initId)
--     condId <- visualise cond
--     emit ("    node" ++ show forId ++ ":f2 -> node" ++ show condId)
--     incrId <- visualise incr
--     emit ("    node" ++ show forId ++ ":f3 -> node" ++ show incrId)
--     bodyId <- visualise body
--     emit ("    node" ++ show forId ++ ":f4 -> node" ++ show bodyId)
--     return forId
--   visualise (If cond body (Just alt)) = do
--     ifId <- nextId
--     emit
--       ( "    node"
--           ++ show ifId
--           ++ "[label=\"<f0>If"
--           ++ " | <f1>"
--           ++ "cond"
--           ++ " | <f2> "
--           ++ "body"
--           ++ " | <f3> "
--           ++ "alt"
--           ++ "\", shape=record]"
--       )
--     condId <- visualise cond
--     emit ("    node" ++ show ifId ++ ":f1 -> node" ++ show condId)
--     bodyId <- visualise body
--     emit ("    node" ++ show ifId ++ ":f2 -> node" ++ show bodyId)
--     altId <- visualise alt
--     emit ("    node" ++ show ifId ++ ":f3 -> node" ++ show altId)
--     return ifId
--   visualise (If cond body Nothing) = do
--     ifId <- nextId
--     emit
--       ( "    node"
--           ++ show ifId
--           ++ "[label=\"<f0>If"
--           ++ " | <f1>"
--           ++ "cond"
--           ++ " | <f2> "
--           ++ "body"
--           ++ "\", shape=record]"
--       )
--     condId <- visualise cond
--     emit ("    node" ++ show ifId ++ ":f1 -> node" ++ show condId)
--     bodyId <- visualise body
--     emit ("    node" ++ show ifId ++ ":f2 -> node" ++ show bodyId)
--     return ifId
--   visualise (Return (Just retVal)) = do
--     returnId <- nextId
--     emit
--       ( "    node"
--           ++ show returnId
--           ++ "[label=\"<f0> Return"
--           ++ " | <f1>"
--           ++ "value"
--           ++ "\", shape=record]"
--       )
--     retValId <- visualise retVal
--     emit ("    node" ++ show returnId ++ ":f1 -> node" ++ show retValId)
--     return returnId
--   visualise (Return Nothing) = do
--     returnId <- nextId
--     emit
--       ( "    node"
--           ++ show returnId
--           ++ "[label=\"<f0> Return"
--           ++ " | <f1>"
--           ++ ";"
--           ++ "\", shape=record]"
--       )
--     return returnId

instance SemantAstDrawable SExpr where
  visualise (typ, SNegative expr) = do
    negativeId <- nextId
    emit
      ( "    node"
          ++ show negativeId
          ++ "[label=\"<f0>"
          ++ "Type: "
          ++ display typ
          ++ " | <f1>"
          ++ "-"
          ++ "\", shape=record]"
      )
    innerId <- visualise expr
    connect negativeId innerId
    return negativeId
  visualise (typ, SNegate expr) = do
    negateId <- nextId
    emit
      ( "    node"
          ++ show negateId
          ++ "[label=\"<f0>"
          ++ "Type: "
          ++ display typ
          ++ " | <f1>"
          ++ "!"
          ++ "\", shape=record]"
      )
    innerId <- visualise expr
    connect negateId innerId
    return negateId
  visualise (typ, SSizeof (Right expr)) = do
    sizeofId <- nextId
    lparenId <- nextId
    rparenId <- nextId
    emit
      ( "    node"
          ++ show sizeofId
          ++ "[label=\"<f0>"
          ++ "Type: "
          ++ display typ
          ++ " | <f1>"
          ++ "Sizeof"
          ++ "\", shape=record]"
      )
    emitNode lparenId "("
    exprId <- visualise expr
    emitNode rparenId ")"
    connect sizeofId lparenId
    connect sizeofId exprId
    connect sizeofId rparenId
    return sizeofId
  visualise (typ, SSizeof (Left typ')) = do
    sizeofId <- nextId
    lparenId <- nextId
    typeId <- nextId
    rparenId <- nextId
    emit
      ( "    node"
          ++ show sizeofId
          ++ "[label=\"<f0>"
          ++ "Type: "
          ++ display typ
          ++ " | <f1>"
          ++ "Sizeof"
          ++ "\", shape=record]"
      )
    emitNode lparenId "("
    emitNode typeId (displayType typ')
    emitNode rparenId ")"
    connect sizeofId lparenId
    connect sizeofId typeId
    connect sizeofId rparenId
    return sizeofId
  visualise (typ, SBinop left op right) = do
    binopId <- nextId
    emit
      ( "    node"
          ++ show binopId
          ++ "[label=\"<f0>"
          ++ "Type: "
          ++ display typ
          ++ " | <f1>"
          ++ displayOp op
          ++ "\", shape=record]"
      )
    leftId <- visualise left
    rightId <- visualise right
    connect binopId leftId
    connect binopId rightId
    return binopId
    where
      displayOp =
        \case
          Add -> "+"
          Sub -> "-"
          Mul -> "*"
          Div -> "/"
          Mod -> "%"
          Equal -> "=="
          Neq -> "!="
          Less -> "\\<"
          Leq -> "\\<="
          Greater -> "\\>"
          Geq -> "\\>="
          And -> "&&"
          Or -> "\\|\\|"
          BitwiseAnd -> "&"
          BitwiseOr -> "\\|"
          BitwiseXor -> "^"
  visualise (typ, LVal name) = do
    litId <- nextId
    emit
      ( "    node"
          ++ show litId
          ++ "[label=\"<f0>"
          ++ "Type: "
          ++ display typ
          ++ " | <f1> "
          ++ "Ident"
          ++ " | <f2> "
          ++ show name
          ++ "\", shape=record]"
      )
    return litId
  visualise (typ, SNull) = do
    nullId <- nextId
    emit
      ( "    node"
          ++ show nullId
          ++ "[label=\"<f0>"
          ++ "Type: "
          ++ display typ
          ++ " | <f1> "
          ++ escape "NULL"
          ++ "\", shape=record]"
      )
    return nullId
  visualise (typ, SLitInt int) = do
    litId <- nextId
    emit
      ( "    node"
          ++ show litId
          ++ "[label=\"<f0>"
          ++ "Type: "
          ++ display typ
          ++ " | <f1> "
          ++ "Literal"
          ++ " | <f2> "
          ++ "int"
          ++ "| <f3> "
          ++ show int
          ++ "\", shape=record]"
      )
    return litId
  visualise (typ, SLitDouble double) = do
    litId <- nextId
    emit
      ( "    node"
          ++ show litId
          ++ "[label=\"<f0>"
          ++ "Type: "
          ++ display typ
          ++ " | <f1> "
          ++ "Literal"
          ++ " | <f2> "
          ++ "double"
          ++ "| <f3> "
          ++ show double
          ++ "\", shape=record]"
      )
    return litId
  visualise (typ, SLitString string) = do
    litId <- nextId
    emit
      ( "    node"
          ++ show litId
          ++ "[label=\"<f0>"
          ++ "Type: "
          ++ display typ
          ++ " | <f1> "
          ++ "Literal"
          ++ " | <f2> "
          ++ "string"
          ++ "| <f3> "
          ++ escape string
          ++ "\", shape=record]"
      )
    return litId
  visualise (typ, SLitChar char) = do
    litId <- nextId
    emit
      ( "    node"
          ++ show litId
          ++ "[label=\"<f0>"
          ++ "Type:"
          ++ display typ
          ++ " | <f1> "
          ++ "Literal"
          ++ " | <f2> "
          ++ "char"
          ++ "|  <f3> "
          ++ "\\\'"
          ++ [char]
          ++ "\\\'"
          ++ "\", shape=record]"
      )
    return litId

instance SemantAstDrawable () where
  visualise _ = gets id

instance SemantAstDrawable Int where
  visualise _ = gets id

instance SemantAstDrawable Char where
  visualise _ = gets id

instance SemantAstDrawable String where
  visualise _ = gets id

display :: Semant.Type.Type -> String
display (Scalar typ) = displayType typ
display (Array typ sizes) = displayType typ ++ show sizes
display Any = "Any"

displayType :: Ast.Type -> String
displayType (PrimitiveType builtin ptrs) = display builtin ++ replicate ptrs '*'
  where
    display L.Int = "int"
    display L.Double = "double"
    display L.Char = "char"
    display L.Void = " void"
displayType (StructType name ptrs) = "struct " ++ name ++ replicate ptrs '*'

escape :: String -> String
escape name = "\\\"" ++ name ++ "\\\""