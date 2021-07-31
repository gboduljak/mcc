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

instance SemantAstDrawable SProgram where
  visualise (SProgram structs funcs globals) = do
    programId <- nextId
    structsId <- nextId
    funcsId <- nextId
    globalsId <- nextId
    emit
      ( "    node"
          ++ show programId
          ++ "[label=<<B>Program</B>>"
          ++ ", shape=record]"
      )
    emit
      ( "    node"
          ++ show structsId
          ++ "[label=\"<f0> Structs"
          ++ "\", shape=record]"
      )
    connect programId structsId
    emit
      ( "    node"
          ++ show funcsId
          ++ "[label=\"<f0> Functions"
          ++ "\", shape=record]"
      )
    connect programId funcsId
    emit
      ( "    node"
          ++ show globalsId
          ++ "[label=\"<f0> Globals"
          ++ "\", shape=record]"
      )
    connect programId globalsId

    traverse_
      ( \struct -> do
          structId <- visualise struct
          connect structsId structId
      )
      structs
    traverse_
      ( \func -> do
          funcId <- visualise func
          connect funcsId funcId
      )
      funcs
    traverse_
      ( \global -> do
          globalId <- visualise global
          connect globalsId globalId
      )
      globals
    return programId

instance SemantAstDrawable SStruct where
  visualise struct@(SStruct name fields _) = do
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
      ( \varDecl@(SVar _ name) -> do
          fieldId <- nextId
          case getFieldOffset name struct of
            (Just offset) -> do
              emit
                ( "    node"
                    ++ show fieldId
                    ++ "[label=\"<f0> Field | <f1>"
                    ++ escape name
                    ++ "@"
                    ++ show offset
                    ++ "\", shape=record]"
                )
              varDeclId <- visualise varDecl
              emit ("    node" ++ show declNodeId ++ ":f2 -> node" ++ show varDeclId ++ ";")
            Nothing -> return ()
      )
      fields

    return declNodeId

instance SemantAstDrawable SFormal where
  visualise (SFormal typ name) = do
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

instance SemantAstDrawable SFunction where
  visualise decl@(SFunction rettyp name formals body) = do
    declNodeId <- nextId
    emit
      ( "    node"
          ++ show declNodeId
          ++ "[label=\"<f0>Function"
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

instance SemantAstDrawable (Maybe SBlock) where
  visualise (Just block) = visualise block
  visualise Nothing = do
    emptyExprId <- nextId
    emit
      ( "    node"
          ++ show emptyExprId
          ++ "[label=\"<f0>"
          ++ escape "nonexistent block"
          ++ "\", shape=record]"
      )
    return emptyExprId

instance SemantAstDrawable SBlock where
  visualise (SBlock stmts) = do
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

instance SemantAstDrawable SVarDecl where
  visualise decl@(SVar Any name) = do
    declNodeId <- nextId
    nameId <- nextId
    emit
      ( "    node"
          ++ show declNodeId
          ++ "[label=\"<f0> VarDecl | <f1>"
          ++ display Any
          ++ " | <f2> "
          ++ escape name
          ++ "\", shape=record]"
      )
    return declNodeId
  visualise decl@(SVar typ name) = do
    declNodeId <- nextId
    nameId <- nextId
    do
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

instance SemantAstDrawable SStatement where
  visualise (SExpr expr) = visualise expr
  visualise (SBlockStatement stmt) = visualise stmt
  visualise (SVarDeclStatement stmt) = visualise stmt
  visualise (SDoWhile cond body) = do
    whileId <- nextId
    emit
      ( "    node"
          ++ show whileId
          ++ "[label=\"<f0>DoWhile"
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
  visualise (SIf cond body (Just alt)) = do
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
  visualise (SIf cond body Nothing) = do
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
  visualise (SReturn expr) = do
    returnId <- nextId
    emit
      ( "    node"
          ++ show returnId
          ++ "[label=\"<f0> Return"
          ++ " | <f1>"
          ++ "value"
          ++ "\", shape=record]"
      )
    retValId <- visualise expr
    emit ("    node" ++ show returnId ++ ":f1 -> node" ++ show retValId)
    return returnId

instance SemantAstDrawable LValue where
  visualise (SDeref expr) = do
    derefId <- nextId
    emit
      ( "    node"
          ++ show derefId
          ++ "[label=\"<f0> Deref\", shape=record]"
      )
    exprId <- visualise expr
    connect derefId exprId
    return derefId
  visualise (SIdent name) = do
    identId <- nextId
    emit
      ( "    node"
          ++ show identId
          ++ "[label=\"<f0>"
          ++ "Ident"
          ++ " | <f1>"
          ++ escape name
          ++ "\", shape=record]"
      )
    return identId
  visualise (SFieldAccess expr fieldName) = do
    accessId <- nextId
    fieldId <- nextId
    dotId <- nextId
    emitNode accessId "FieldAccess"
    innerId <- visualise expr
    emitNode dotId "."
    emitNode fieldId (escape fieldName)
    connect accessId innerId
    connect accessId dotId
    connect accessId fieldId
    return accessId
  visualise (SArrayAccess target indices) = do
    accessId <- nextId
    emitNode accessId "ArrayAccess"
    targetId <- visualise target
    traverse_
      ( \indexExpr -> do
          lbrackId <- nextId
          rbrackId <- nextId
          emitNode lbrackId "["
          indexId <- visualise indexExpr
          emitNode rbrackId "]"
          connect accessId lbrackId
          connect accessId indexId
          connect accessId rbrackId
      )
      indices
    connect accessId targetId
    return accessId

instance SemantAstDrawable SExpr where
  visualise (typ, SCall func actuals) = do
    callId <- nextId
    nameId <- nextId
    lparenId <- nextId
    rparenId <- nextId
    emit
      ( "    node"
          ++ show callId
          ++ "[label=\"<f0>"
          ++ "Type: "
          ++ display typ
          ++ " | <f1> "
          ++ "Call"
          ++ "\", shape=record]"
      )
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
  visualise (typ, SAssign lValue expr) = do
    assignId <- nextId
    equalId <- nextId
    emit
      ( "    node"
          ++ show assignId
          ++ "[label=\"<f0>"
          ++ "Type: "
          ++ display typ
          ++ " | <f1> "
          ++ "Assign"
          ++ "\", shape=record]"
      )
    targetId <- visualise lValue
    emitNode equalId "="
    valueId <- visualise expr
    connect assignId targetId
    connect assignId equalId
    connect assignId valueId
    return assignId
  visualise (typ, LVal expr) = do
    typedLValId <- nextId
    emit
      ( "    node"
          ++ show typedLValId
          ++ "[label=\"<f0>"
          ++ "Type: "
          ++ display typ
          ++ " | <f1> "
          ++ "LValue"
          ++ "\", shape=record]"
      )
    lValId <- visualise expr
    connect typedLValId lValId
    return typedLValId
  visualise (typ, STypecast targetType expr) = do
    typecastId <- nextId
    lparenId <- nextId
    typeId <- nextId
    rparenId <- nextId
    emit
      ( "    node"
          ++ show typecastId
          ++ "[label=\"<f0>"
          ++ "Type: "
          ++ display typ
          ++ " | <f1> "
          ++ "Typecast"
          ++ "\", shape=record]"
      )
    emitNode lparenId "("
    emitNode typeId (displayType targetType)
    emitNode rparenId ")"
    exprId <- visualise expr
    connect typecastId lparenId
    connect typecastId typeId
    connect typecastId rparenId
    connect typecastId exprId
    return typecastId
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
  visualise (typ, SAddressOf expr) = do
    addressOfId <- nextId
    emit
      ( "    node"
          ++ show addressOfId
          ++ "[label=\"<f0>"
          ++ "Type: "
          ++ display typ
          ++ " | <f1>"
          ++ "AddressOf"
          ++ "\", shape=record]"
      )
    innerId <- visualise expr
    connect addressOfId innerId
    return addressOfId
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
  visualise (typ, SEmptyExpr) = do
    emptyExprId <- nextId
    emit
      ( "    node"
          ++ show emptyExprId
          ++ "[label=\"<f0>"
          ++ "Type: "
          ++ display typ
          ++ " | <f1> "
          ++ escape "empty expr"
          ++ "\", shape=record]"
      )
    return emptyExprId
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
