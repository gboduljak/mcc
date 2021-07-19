{-# LANGUAGE LambdaCase #-}

module Parser.AstPrettyPrinter where

import Data.Text.Prettyprint.Doc
import Parser.Ast

indentAmount :: Int
indentAmount = 4

instance Pretty Formal where
  pretty (Formal typ name) = pretty typ <+> pretty name

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

instance Pretty Expr where
  pretty (LitInt x) = pretty x
  pretty (LitDouble x) = pretty x
  pretty (LitString x) = pretty "\"" <> pretty x <> pretty "\""
  pretty (LitChar x) = pretty "\'" <> pretty x <> pretty "\'"
  pretty (Null) = pretty "NULL"
  pretty (Ident x) = pretty x
  pretty (Nested expr) = pretty "(" <> pretty expr <> pretty ")"
  pretty (Binop left op right) = pretty left <+> pretty op <+> pretty right
  pretty (Deref expr) = pretty "*" <> parens (pretty expr)
  pretty (AddressOf expr) = pretty "&" <> parens (pretty expr)
  pretty (Negate expr) = pretty "!" <> parens (pretty expr)
  pretty (Negative expr) = pretty "-" <> parens (pretty expr)
  pretty (FieldAccess expr field) = pretty expr <> pretty "." <> pretty field
  pretty (ArrayAccess expr index) = pretty expr <> pretty "[" <> pretty index <> pretty "]"
  pretty (Indirect expr field) = pretty expr <> pretty "->" <> pretty field
  pretty (Sizeof (Left typ)) = pretty "sizeof" <> parens (pretty typ) <> pretty ")"
  pretty (Sizeof (Right expr)) = pretty "sizeof" <> pretty expr <> pretty ")"
  pretty (Typecast typ expr) = pretty "(" <> pretty typ <> pretty ")" <> pretty expr
  pretty (Call function actuals) = pretty function <> (tupled . map pretty) actuals
  pretty (Assign left value) = pretty left <+> pretty "=" <+> pretty value

instance Pretty VarDecl where
  pretty (Var typ name ptrs) =
    pretty typ <+> pretty name
      <> hcat [pretty '[' <> pretty x <> pretty ']' | x <- ptrs]
      <> pretty ';'

instance Pretty StructDecl where
  pretty (Struct name varDecls) =
    pretty "struct" <+> pretty name <+> lbrace
      <> hardline
      <> indent indentAmount (vsep [pretty decl | decl <- varDecls])
      <> hardline
      <> rbrace
      <> pretty ";"

instance Pretty Block where
  pretty (Block stmts) = (vsep . map pretty) stmts

instance Pretty Statement where
  pretty (Expr expr) = pretty expr <> semi
  pretty (BlockStatement block) =
    lbrace
      <> hardline
      <> indent indentAmount (pretty block)
      <> hardline
      <> rbrace
  pretty (VarDeclStatement vdecl) = pretty vdecl
  pretty (While cond body) = pretty "while" <+> parens (pretty cond) <+> pretty body
  pretty (For init cond incr body) =
    pretty "for"
      <+> encloseSep lparen rparen semi (map pretty [init, cond, incr])
      <+> pretty body
  pretty (If pred cons alt) =
    pretty "if"
      <+> parens (pretty pred)
      <+> pretty cons
      <+> prettyAlt alt
    where
      prettyAlt Nothing = mempty
      prettyAlt (Just alt) = hardline <> pretty "else" <+> pretty alt
  pretty (Return expr) = pretty "return" <+> pretty expr <> semi

instance Pretty FuncDecl where
  pretty (Func rettyp name formals body) =
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
  pretty (VarDecl vdecl) = pretty vdecl

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