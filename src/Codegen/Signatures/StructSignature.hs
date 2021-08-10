{-# LANGUAGE RecordWildCards #-}
module Codegen.Signatures.StructSignature 
where
import qualified LLVM.AST
import Data.Maybe (fromJust)
import qualified Data.Set as Map
import Data.List (find)
import qualified Semant.Type
import LLVM.AST (Name)
import LLVM.AST.Name (mkName)

data FieldSignature = FieldSignature {
  fieldName :: String,
  llvmFieldType :: LLVM.AST.Type,
  semantFieldType :: Semant.Type.Type,
  fieldOffset :: Int
} deriving (Eq, Show)

data StructSignature = StructSignature {
  name :: Name,
  fields :: [FieldSignature]
} deriving (Eq, Show)

llvmStructName :: String -> Name
llvmStructName struct = mkName ("struct." ++ struct)

getFieldOffset :: String -> StructSignature -> Int
getFieldOffset field StructSignature{..} = fieldOffset . fromJust $ find (
  \FieldSignature{..} -> fieldName == field) 
  fields

getLLVMFieldType :: String -> StructSignature -> LLVM.AST.Type 
getLLVMFieldType field StructSignature{..} = llvmFieldType . fromJust $ find (
  \FieldSignature{..} -> fieldName == field) 
  fields

getSemantFieldType :: String -> StructSignature -> LLVM.AST.Type 
getSemantFieldType field StructSignature{..} = llvmFieldType . fromJust $ find (
  \FieldSignature{..} -> fieldName == field) 
  fields