{-# LANGUAGE RecordWildCards #-}
module Codegen.Signatures.StructSignature 
where
import qualified LLVM.AST
import Data.Maybe (fromJust)
import qualified Data.Set as Map
import Data.List (find)

data FieldSignature = FieldSignature {
  fieldName :: String,
  fieldType :: LLVM.AST.Type,
  fieldOffset :: Int
} deriving (Eq, Show)

data StructSignature = StructSignature {
  name :: String,
  fields :: [FieldSignature]
} deriving (Eq, Show)


getFieldOffset :: String -> StructSignature -> Int
getFieldOffset field StructSignature{..} = fieldOffset . fromJust $ find (
  \FieldSignature{..} -> fieldName == field) 
  fields

getFieldType :: String -> StructSignature -> LLVM.AST.Type 
getFieldType field StructSignature{..} = fieldType . fromJust $ find (
  \FieldSignature{..} -> fieldName == field) 
  fields