{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Parser.AstVisualiser where

import Control.Monad.State (MonadState (get), State, gets, modify, runState)
import Data.Foldable (traverse_)
import Data.List
import qualified Lexer.Lexeme as L
import Parser.Ast
import Prelude hiding (id)

class AstDrawable a where
  visualise :: AstDrawable () => a -> AstVisualiser Int

data AstVisualiserState = AstVisualiserState Int [String]

type AstVisualiser a = AstDrawable a => State AstVisualiserState a

id :: AstVisualiserState -> Int
id (AstVisualiserState nodeId output) = nodeId

output :: AstVisualiserState -> [String]
output (AstVisualiserState nodeId output) = output

connect :: Int -> Int -> AstVisualiser ()
connect u v = emit ("    node" ++ show u ++ "->" ++ "node" ++ show v)

emit :: String -> AstVisualiser ()
emit line =
  modify (\(AstVisualiserState nodeId output) -> AstVisualiserState nodeId (line : output))

emitNode :: Int -> String -> AstVisualiser ()
emitNode id label = emit ("    node" ++ show id ++ "[label=\"" ++ label ++ "\"]")

nextId :: AstVisualiser Int
nextId = do
  id <- gets id
  modify (\(AstVisualiserState nodeId output) -> AstVisualiserState (nodeId + 1) output)
  return id

reverseOutput :: AstVisualiser ()
reverseOutput = modify (\(AstVisualiserState nodeId output) -> AstVisualiserState nodeId (reverse output))

visualiseAst :: AstDrawable a => a -> String
visualiseAst ast = intercalate "\n" (output state)
  where
    (_, state) = runState visualise' (AstVisualiserState 0 [])
    visualise' = do
      emit "digraph g {"
      visualise ast
      emit "}"
      reverseOutput

instance AstDrawable Program where
  visualise (Program includes constructs) = do
    programId <- nextId
    includesId <- nextId
    emit
      ( "    node"
          ++ show programId
          ++ "[label=<<B>Program</B>>"
          ++ ", shape=record]"
      )
    emit
      ( "    node"
          ++ show includesId
          ++ "[label=\"<f0> Includes"
          ++ "\", shape=record]"
      )
    connect programId includesId
    traverse_
      ( \include -> do
          includeId <- visualise include
          connect includesId includeId
      )
      includes
    traverse_
      ( \constr -> do
          constrId <- visualise constr
          connect programId constrId
      )
      constructs
    return programId

instance AstDrawable Construct where
  visualise (FuncDecl decl) = visualise decl
  visualise (FuncDefn defn) = visualise defn
  visualise (StructDecl decl) = visualise decl
  visualise (VarDecl decl) = visualise decl

instance AstDrawable Directive where
  visualise (Include file) = do
    includeId <- nextId
    emit
      ( "    node"
          ++ show includeId
          ++ "[label=\"<f0>"
          ++ "#include"
          ++ " | <f1> "
          ++ escape file
          ++ "\", shape=record]"
      )
    return includeId

instance AstDrawable VarDecl where
  visualise decl@(Var typ name arraySizes) = do
    declNodeId <- nextId
    nameId <- nextId
    if isArray decl
      then do
        let sizes = concat ["[" ++ show size ++ "]" | size <- arraySizes]
        emit
          ( "    node"
              ++ show declNodeId
              ++ "[label=\"<f0> VarDecl | <f1>"
              ++ display typ
              ++ " | <f2> "
              ++ escape name
              ++ " | <f3> "
              ++ sizes
              ++ "\", shape=record]"
          )
        return declNodeId
      else do
        emit
          ( "    node"
              ++ show declNodeId
              ++ "[label=\"<f0> VarDecl | <f1>"
              ++ display typ
              ++ " | <f2> "
              ++ escape name
              ++ "\", shape=record]"
          )
        return declNodeId

instance AstDrawable StructDecl where
  visualise decl@(Struct name vardecls) = do
    declNodeId <- nextId
    emit
      ( "    node"
          ++ show declNodeId
          ++ "[label=\"<f0> StructDecl | <f1>"
          ++ escape name
          ++ " | <f2> "
          ++ "fields"
          ++ "\", shape=record]"
      )
    traverse_
      ( \varDecl -> do
          varDeclId <- visualise varDecl
          emit ("    node" ++ show declNodeId ++ ":f2 -> node" ++ show varDeclId ++ ";")
      )
      vardecls

    return declNodeId

instance AstDrawable Formal where
  visualise (Formal typ name) = do
    formalId <- nextId
    emit
      ( "    node"
          ++ show formalId
          ++ "[label=\"<f0>"
          ++ display typ
          ++ " | <f1> "
          ++ escape name
          ++ "\", shape=record]"
      )
    return formalId

instance AstDrawable FuncDef where
  visualise decl@(FuncDef rettyp name formals body) = do
    declNodeId <- nextId
    emit
      ( "    node"
          ++ show declNodeId
          ++ "[label=\"<f0>FuncDef"
          ++ " | <f1>"
          ++ display rettyp
          ++ " | <f2> "
          ++ escape name
          ++ " | <f3> "
          ++ "formals"
          ++ " | <f4> "
          ++ "body"
          ++ "\", shape=record]"
      )
    traverse_
      ( \formal -> do
          formalId <- visualise formal
          emit ("    node" ++ show declNodeId ++ ":f3 -> node" ++ show formalId)
      )
      formals

    bodyId <- visualise body
    emit ("    node" ++ show declNodeId ++ ":f4 -> node" ++ show bodyId)

    return declNodeId

instance AstDrawable FuncDecl where
  visualise decl@(Func rettyp name formals) = do
    declNodeId <- nextId
    emit
      ( "    node"
          ++ show declNodeId
          ++ "[label=\"<f0>FuncDecl"
          ++ " | <f1>"
          ++ display rettyp
          ++ " | <f2> "
          ++ escape name
          ++ " | <f3> "
          ++ "formals"
          ++ "\", shape=record]"
      )
    traverse_
      ( \formal -> do
          formalId <- visualise formal
          emit ("    node" ++ show declNodeId ++ ":f3 -> node" ++ show formalId)
      )
      formals

    return declNodeId

instance AstDrawable Block where
  visualise (Block stmts) = do
    blockId <- nextId
    lbraceId <- nextId
    rbraceId <- nextId
    emit
      ( "    node"
          ++ show blockId
          ++ "[label=\"<f0>"
          ++ "Block"
          ++ "\", shape=record]"
      )
    emitNode lbraceId "{"
    connect blockId lbraceId
    traverse_
      ( \stmt -> do
          stmtId <- visualise stmt
          connect blockId stmtId
      )
      stmts
    emitNode rbraceId "}"
    connect blockId rbraceId
    return blockId

instance AstDrawable (Maybe Expr) where
  visualise (Just expr) = visualise expr
  visualise Nothing = do
    emptyId <- nextId
    emitNode emptyId "{}"
    return emptyId

instance AstDrawable Statement where
  visualise (Expr expr) = visualise expr
  visualise (BlockStatement stmt) = visualise stmt
  visualise (VarDeclStatement stmt) = visualise stmt
  visualise (While cond body) = do
    whileId <- nextId
    emit
      ( "    node"
          ++ show whileId
          ++ "[label=\"<f0>While"
          ++ " | <f1>"
          ++ "cond"
          ++ " | <f2> "
          ++ "body"
          ++ "\", shape=record]"
      )
    condId <- visualise cond
    emit ("    node" ++ show whileId ++ ":f1 -> node" ++ show condId)
    bodyId <- visualise body
    emit ("    node" ++ show whileId ++ ":f2 -> node" ++ show bodyId)
    return whileId
  visualise (For init cond incr body) = do
    forId <- nextId
    emit
      ( "    node"
          ++ show forId
          ++ "[label=\"<f0>For"
          ++ " | <f1>"
          ++ "init"
          ++ " | <f2> "
          ++ "cond"
          ++ " | <f3> "
          ++ "incr"
          ++ " | <f4> "
          ++ "body"
          ++ "\", shape=record]"
      )
    initId <- visualise init
    emit ("    node" ++ show forId ++ ":f1 -> node" ++ show initId)
    condId <- visualise cond
    emit ("    node" ++ show forId ++ ":f2 -> node" ++ show condId)
    incrId <- visualise incr
    emit ("    node" ++ show forId ++ ":f3 -> node" ++ show incrId)
    bodyId <- visualise body
    emit ("    node" ++ show forId ++ ":f4 -> node" ++ show bodyId)
    return forId
  visualise (If cond body (Just alt)) = do
    ifId <- nextId
    emit
      ( "    node"
          ++ show ifId
          ++ "[label=\"<f0>If"
          ++ " | <f1>"
          ++ "cond"
          ++ " | <f2> "
          ++ "body"
          ++ " | <f3> "
          ++ "alt"
          ++ "\", shape=record]"
      )
    condId <- visualise cond
    emit ("    node" ++ show ifId ++ ":f1 -> node" ++ show condId)
    bodyId <- visualise body
    emit ("    node" ++ show ifId ++ ":f2 -> node" ++ show bodyId)
    altId <- visualise alt
    emit ("    node" ++ show ifId ++ ":f3 -> node" ++ show altId)
    return ifId
  visualise (If cond body Nothing) = do
    ifId <- nextId
    emit
      ( "    node"
          ++ show ifId
          ++ "[label=\"<f0>If"
          ++ " | <f1>"
          ++ "cond"
          ++ " | <f2> "
          ++ "body"
          ++ "\", shape=record]"
      )
    condId <- visualise cond
    emit ("    node" ++ show ifId ++ ":f1 -> node" ++ show condId)
    bodyId <- visualise body
    emit ("    node" ++ show ifId ++ ":f2 -> node" ++ show bodyId)
    return ifId
  visualise (Return (Just retVal)) = do
    returnId <- nextId
    emit
      ( "    node"
          ++ show returnId
          ++ "[label=\"<f0> Return"
          ++ " | <f1>"
          ++ "value"
          ++ "\", shape=record]"
      )
    retValId <- visualise retVal
    emit ("    node" ++ show returnId ++ ":f1 -> node" ++ show retValId)
    return returnId
  visualise (Return Nothing) = do
    returnId <- nextId
    emit
      ( "    node"
          ++ show returnId
          ++ "[label=\"<f0> Return"
          ++ " | <f1>"
          ++ ";"
          ++ "\", shape=record]"
      )
    return returnId

instance AstDrawable Expr where
  visualise (Assign target value) = do
    assignId <- nextId
    equalId <- nextId
    emitNode assignId "Assign"
    targetId <- visualise target
    emitNode equalId "="
    valueId <- visualise value
    connect assignId targetId
    connect assignId equalId
    connect assignId valueId
    return assignId
  visualise (Call func actuals) = do
    callId <- nextId
    nameId <- nextId
    lparenId <- nextId
    rparenId <- nextId
    emitNode callId "Call"
    emitNode nameId (escape func)
    emitNode lparenId "("
    connect callId nameId
    connect callId lparenId
    traverse_
      ( \act -> do
          actId <- visualise act
          connect callId actId
      )
      actuals
    emitNode rparenId ")"
    connect callId rparenId
    return callId
  visualise (Typecast typ expr) = do
    typecastId <- nextId
    lparenId <- nextId
    typeId <- nextId
    rparenId <- nextId
    emitNode typecastId "Typecast"
    emitNode lparenId "("
    emitNode typeId (display typ)
    emitNode rparenId ")"
    exprId <- visualise expr
    connect typecastId lparenId
    connect typecastId typeId
    connect typecastId rparenId
    connect typecastId exprId
    return typecastId
  visualise (Sizeof (Right expr)) = do
    sizeofId <- nextId
    lparenId <- nextId
    rparenId <- nextId
    emitNode sizeofId "Sizeof"
    emitNode lparenId "("
    exprId <- visualise expr
    emitNode rparenId ")"
    connect sizeofId lparenId
    connect sizeofId exprId
    connect sizeofId rparenId
    return sizeofId
  visualise (Sizeof (Left typ)) = do
    sizeofId <- nextId
    lparenId <- nextId
    typeId <- nextId
    rparenId <- nextId
    emitNode sizeofId "Sizeof"
    emitNode lparenId "("
    emitNode typeId (display typ)
    emitNode rparenId ")"
    connect sizeofId lparenId
    connect sizeofId typeId
    connect sizeofId rparenId
    return sizeofId
  visualise (Indirect target field) = do
    indirectId <- nextId
    fieldId <- nextId
    arrowId <- nextId
    emitNode indirectId "Indirect"
    innerId <- visualise target
    emitNode arrowId "->"
    emitNode fieldId (escape field)
    connect indirectId innerId
    connect indirectId arrowId
    connect indirectId fieldId
    return indirectId
  visualise (ArrayAccess target index) = do
    accessId <- nextId
    lbrackId <- nextId
    rbrackId <- nextId
    emitNode accessId "ArrayAccess"
    targetId <- visualise target
    emitNode lbrackId "["
    indexId <- visualise index
    emitNode rbrackId "]"
    connect accessId targetId
    connect accessId lbrackId
    connect accessId indexId
    connect accessId rbrackId
    return accessId
  visualise (FieldAccess expr field) = do
    accessId <- nextId
    fieldId <- nextId
    dotId <- nextId
    emitNode accessId "FieldAccess"
    innerId <- visualise expr
    emitNode dotId "."
    emitNode fieldId (escape field)
    connect accessId innerId
    connect accessId dotId
    connect accessId fieldId
    return accessId
  visualise (Negative expr) = do
    negativeId <- nextId
    emitNode negativeId "-"
    innerId <- visualise expr
    connect negativeId innerId
    return negativeId
  visualise (Negate expr) = do
    negateId <- nextId
    emitNode negateId "!"
    innerId <- visualise expr
    connect negateId innerId
    return negateId
  visualise (AddressOf expr) = do
    addressOfId <- nextId
    emitNode addressOfId "AddressOf"
    innerId <- visualise expr
    connect addressOfId innerId
    return addressOfId
  visualise (Deref expr) = do
    derefId <- nextId
    emitNode derefId "Deref"
    innerId <- visualise expr
    connect derefId innerId
    return derefId
  visualise (Binop left op right) = do
    binopId <- nextId
    emitNode binopId (display op)
    leftId <- visualise left
    rightId <- visualise right
    connect binopId leftId
    connect binopId rightId
    return binopId
    where
      display = \case
        Add -> "+"
        Sub -> "-"
        Mul -> "*"
        Div -> "/"
        Mod -> "%"
        Equal -> "=="
        Neq -> "!="
        Less -> "<"
        Leq -> "<="
        Greater -> ">"
        Geq -> ">="
        And -> "&&"
        Or -> "||"
        BitwiseAnd -> "&"
        BitwiseOr -> "|"
        BitwiseXor -> "^"
  visualise (Nested expr) = do
    nestedId <- nextId
    lparenId <- nextId
    rparenId <- nextId
    emitNode nestedId "Nested"
    emitNode lparenId "("
    exprId <- visualise expr
    emitNode rparenId ")"
    connect nestedId lparenId
    connect nestedId exprId
    connect nestedId rparenId
    return nestedId
  visualise (Ident name) = do
    litId <- nextId
    emit
      ( "    node"
          ++ show litId
          ++ "[label=\"<f0>"
          ++ "Ident"
          ++ " | <f1> "
          ++ escape name
          ++ "\", shape=record]"
      )
    return litId
  visualise Null = do
    nullId <- nextId
    emit
      ( "    node"
          ++ show nullId
          ++ "[label=\"<f0>"
          ++ escape "NULL"
          ++ "\", shape=record]"
      )
    return nullId
  visualise (LitInt int) = do
    litId <- nextId
    emit
      ( "    node"
          ++ show litId
          ++ "[label=\"<f0>"
          ++ "Literal"
          ++ " | <f1> "
          ++ "int"
          ++ "| <f2> "
          ++ show int
          ++ "\", shape=record]"
      )
    return litId
  visualise (LitDouble double) = do
    litId <- nextId
    emit
      ( "    node"
          ++ show litId
          ++ "[label=\"<f0>"
          ++ "Literal"
          ++ " | <f1> "
          ++ "double"
          ++ "| <f2> "
          ++ show double
          ++ "\", shape=record]"
      )
    return litId
  visualise (LitString string) = do
    litId <- nextId
    emit
      ( "    node"
          ++ show litId
          ++ "[label=\"<f0>"
          ++ "Literal"
          ++ " | <f1> "
          ++ "string"
          ++ "| <f2> "
          ++ escape string
          ++ "\", shape=record]"
      )
    return litId
  visualise (LitChar char) = do
    litId <- nextId
    emit
      ( "    node"
          ++ show litId
          ++ "[label=\"<f0>"
          ++ "Literal"
          ++ " | <f1> "
          ++ "char"
          ++ "| <f2> "
          ++ "\\\'"
          ++ [char]
          ++ "\\\'"
          ++ "\", shape=record]"
      )
    return litId

instance AstDrawable () where
  visualise _ = gets id

instance AstDrawable Int where
  visualise _ = gets id

instance AstDrawable Char where
  visualise _ = gets id

instance AstDrawable String where
  visualise _ = gets id

display :: Type -> [Char]
display (PrimitiveType builtin ptrs) = display builtin ++ replicate ptrs '*'
  where
    display L.Int = "int"
    display L.Double = "double"
    display L.Char = "char"
    display L.Void = " void"
display (StructType name ptrs) = "struct " ++ name ++ replicate ptrs '*'

escape :: String -> String
escape name = "\\\"" ++ name ++ "\\\""