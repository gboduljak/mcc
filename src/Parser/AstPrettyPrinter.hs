{-# LANGUAGE LambdaCase #-}

module Parser.AstPrettyPrinter where

import Data.Text.Prettyprint.Doc
import Parser.Ast

indentAmount :: Int
indentAmount = 4

instance Pretty Formal where
  pretty (Formal typ name _) = pretty typ <+> pretty name
  pretty (FormalError _) = pretty "formal error"

instance Pretty InfixOp where
  pretty = \case
    Add -> pretty "+"
    Sub -> pretty "-"
    Mul -> pretty "*"
    Div -> pretty "/"
    Mod -> pretty "%"
    Equal -> pretty "=="
    Neq -> pretty "!="
    Less -> pretty "<"
    Leq -> pretty "<="
    Greater -> pretty ">"
    Geq -> pretty ">="
    And -> pretty "&&"
    Or -> pretty "||"
    BitwiseAnd -> pretty "&"
    BitwiseOr -> pretty "|"
    BitwiseXor -> pretty "^"

instance Pretty Type where
  pretty (PrimitiveType builtin ptrs) = pretty builtin <> pretty (replicate ptrs '*')
  pretty (StructType name ptrs) = pretty name <> pretty (replicate ptrs '*')

instance Pretty SizeofType where 
  pretty (SizeofType typ sizes) = pretty typ <> pretty displayedSizes
    where displayedSizes = concat ["[" ++ show size ++ "]" | size <- sizes]
  
instance Pretty Expr where
  pretty (LitInt x _) = pretty x
  pretty (LitDouble x _) = pretty x
  pretty (LitString x _) = pretty "\"" <> pretty x <> pretty "\""
  pretty (LitChar x _) = pretty "\'" <> pretty x <> pretty "\'"
  pretty (Null _) = pretty "NULL"
  pretty (Ident x _) = pretty x
  pretty (Nested expr _) = pretty "(" <> pretty expr <> pretty ")"
  pretty (Binop left op right _) = pretty left <+> pretty op <+> pretty right
  pretty (Deref expr _) = pretty "*" <> parens (pretty expr)
  pretty (AddressOf expr _) = pretty "&" <> parens (pretty expr)
  pretty (Negate expr _) = pretty "!" <> parens (pretty expr)
  pretty (Negative expr _) = pretty "-" <> parens (pretty expr)
  pretty (FieldAccess expr field _) = pretty expr <> pretty "." <> pretty field
  pretty (ArrayAccess expr index _) = pretty expr <> pretty "[" <> pretty index <> pretty "]"
  pretty (Indirect expr field _) = pretty expr <> pretty "->" <> pretty field
  pretty (Sizeof (Left typ) _) = pretty "sizeof" <> parens (pretty typ) 
  pretty (Sizeof (Right expr) _) = pretty "sizeof" <> parens (pretty expr)
  pretty (Typecast typ expr _) = parens (pretty typ) <> pretty expr
  pretty (Call function actuals _) = pretty function <> (tupled . map pretty) actuals
  pretty (Assign left value _) = pretty left <+> pretty "=" <+> pretty value
  pretty (ExprError _) = pretty "expr error"

instance Pretty VarDecl where
  pretty (Var typ name ptrs _) =
    pretty typ <+> pretty name
      <> hcat [pretty '[' <> pretty x <> pretty ']' | x <- ptrs]
      <> semi
  pretty (VarDeclError _) = pretty "var decl error" <> semi

instance Pretty StructDecl where
  pretty (Struct name varDecls _) =
    pretty "struct" <+> pretty name <+> lbrace
      <> hardline
      <> indent indentAmount (vsep [pretty decl | decl <- varDecls])
      <> hardline
      <> rbrace
      <> semi
      <> hardline

instance Pretty Block where
  pretty (Block stmts _) = (vsep . map pretty) stmts

instance Pretty Statement where
  pretty (Expr expr _) = pretty expr <> semi
  pretty (BlockStatement block _) =
    lbrace
      <> hardline
      <> indent indentAmount (pretty block)
      <> hardline
      <> rbrace
  pretty (VarDeclStatement vdecl _) = pretty vdecl
  pretty (While cond body _) = pretty "while" <+> parens (pretty cond) <+> pretty body
  pretty (For init cond incr body _) =
    pretty "for"
      <+> encloseSep lparen rparen semi (map pretty [init, cond, incr])
      <+> pretty body
  pretty (If pred cons alt _) =
    pretty "if"
      <+> parens (pretty pred)
      <+> pretty cons
      <+> prettyAlt alt
    where
      prettyAlt Nothing = mempty
      prettyAlt (Just alt) = hardline <> pretty "else" <+> pretty alt
  pretty (Return expr _) = pretty "return" <+> pretty expr <> semi
  pretty (StatementError _) = pretty "statement error" <> semi

instance Pretty FuncDecl where
  pretty (Func rettyp name formals _) =
    pretty rettyp <+> pretty name
      <+> tupled (map pretty formals)
      <> semi
      <> hardline

instance Pretty FuncDef where
  pretty (FuncDef rettyp name formals body _) =
    pretty rettyp <+> pretty name
      <+> tupled (map pretty formals)
      <> hardline
      <> lbrace
      <> hardline
      <> indent indentAmount (pretty body)
      <> hardline
      <> rbrace
      <> hardline

instance Pretty Construct where
  pretty (StructDecl sdecl) = pretty sdecl
  pretty (FuncDecl fdecl) = pretty fdecl
  pretty (FuncDefn fdefn) = pretty fdefn
  pretty (VarDecl vdecl) = pretty vdecl
  pretty ConstructError = pretty "construct error"

instance Pretty Directive where
  pretty (Include file) =
    pretty "#include"
      <+> enclose (pretty "\"") (pretty "\"") (pretty file)

instance Pretty Program where
  pretty (Program includes constructs) = hardsep [prettyIncludes, prettyConstructs]
    where
      prettyIncludes = hardsep (map pretty includes)
      prettyConstructs = hardsep (map pretty constructs)
      hardsep = concatWith (\x y -> x <> hardline <> y)