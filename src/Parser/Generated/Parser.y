
{
module Parser.Generated.Parser where
import qualified Lexer.Lexeme as L
import qualified Parser.Ast as P
import Data.Text (pack)
import Prelude hiding (fst, snd)
}

%name parse
%tokentype { L.Lexeme }
%error { parseError }

%token
  int     { L.LitInt $$ }
  double  { L.LitDouble $$ }
  string  { L.LitString $$ }
  char    { L.LitChar $$ }
  null    { L.LitNull }
  ident   { L.Ident $$ }
  tint    { L.Type L.Int    }
  tdouble { L.Type L.Double }
  tchar   { L.Type L.Char   }
  tvoid   { L.Type L.Void   }
  struct { L.Struct }
  return { L.Return }
  '='    { L.Assign }
  ','    { L.Comma }
  ';'    { L.Semi }
  '('    { L.LParen }
  ')'    { L.RParen }
  '{'    { L.LBrace }
  '}'    { L.RBrace }
  '['    { L.LBrack }
  ']'    { L.RBrack }
  for    { L.For }
  while  { L.While }
  if     { L.If }
  else   { L.Else }
  '+'    { L.Plus }
  '-'    { L.Minus }
  '*'    { L.Asterisk }
  '/'    { L.Div }
  '%'    { L.Mod }
  '=='   { L.Equal }
  '!='   { L.Neq }
  '<'    { L.Less }
  '<='   { L.Leq }
  '>'    { L.Greater }
  '>='   { L.Geq }
  '&&'   { L.And }
  '||'   { L.Or  }
  '!'    { L.Not }
  '&'    { L.Ampers }
  '|'    { L.Bar  }
  '^'    { L.Caret }
  '.'    { L.Dot }
  '->'   { L.Arrow }
  '++'   { L.Increment }
  '--'   { L.Decrement }
  sizeof { L.Sizeof }
  include { L.Include }
  eof { L.Eof }

%right '='
%left '||'
%left '&&'
%left '|'
%left '^'
%left '&'
%left '==' '!='
%left '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/' '%'
%right CAST
%left '!' NEG
%left '.' '->' '++' '--' '[' 
%nonassoc NOELSE
%nonassoc else
%%

program: includes construct_list eof { P.Program (reverse $1) (reverse $2) }

construct_list: 
   {- empty -} { [] }
   | construct_list construct { ($2 : $1) }

construct: funcdecl   { P.FuncDecl $1 }
         | funcdefn   { P.FuncDefn $1 }
         | structdecl { P.StructDecl $1 }
         | vardecl    { P.VarDecl $1 }
         
structdecl: struct ident '{' vardecl_list '}' ';' { P.Struct $2 (reverse $4) 0 }

funcdecl: type ident '(' ')' ';' { P.Func $1 $2 [] 0 }
        | type ident '(' formals ')' ';' { P.Func $1 $2 (reverse $4) 0 }

funcdefn: type ident '(' ')' block { P.FuncDef $1 $2 [] $5 0 }
        | type ident '(' formals ')' block { P.FuncDef $1 $2 (reverse $4) $6 0 }

vardecl: type ident array_sizes ';' { P.Var $1 $2 (reverse $3) 0 }

vardecl_list: vardecl_list vardecl { $2 : $1 }
            | vardecl { [$1] }

formals: formal { [$1] }
       | formals ',' formal { $3 : $1 }
formal: type ident { P.Formal $1 $2 0 }

block: '{' block_statements '}' { P.Block (reverse $2) 0 }

block_statements: {- empty -} { [] }
                | block_statements statement { $2 : $1 }

statement: expr ';' { P.Expr $1 0 }
         | block { P.BlockStatement $1 0 }
         | while '(' expr ')' statement { P.While $3 $5 0 }
         | for '(' opt_expr ';' opt_expr ';' opt_expr ')' statement { P.For $3 $5 $7 $9 0 }
         | if '(' expr ')' statement else statement { P.If $3 $5 (Just $7) 0 }
         | if '(' expr ')' statement %prec NOELSE { P.If $3 $5 Nothing 0 }
         | return opt_expr ';'{ P.Return $2 0 }
         | vardecl { P.VarDeclStatement $1 0 }

expr:
    int                    { P.LitInt $1 0 }
  | double                 { P.LitDouble $1 0 }
  | char                   { P.LitChar $1 0 }
  | string                 { P.LitString $1 0 }
  | null                   { P.Null 0 }
  | ident                  { P.Ident $1 0 }
  | expr '=' expr          { P.Assign $1 $3 0 }
  | expr '||' expr         { P.Binop $1 P.Or $3 0}
  | expr '&&' expr         { P.Binop $1 P.And $3 0 }
  | expr '|' expr          { P.Binop $1 P.BitwiseOr $3 0 }
  | expr '^' expr          { P.Binop $1 P.BitwiseXor $3 0 }
  | expr '&' expr          { P.Binop $1 P.BitwiseAnd $3 0 }
  | expr '==' expr         { P.Binop $1 P.Equal $3 0 }
  | expr '!=' expr         { P.Binop $1 P.Neq $3 0 }
  | expr '>=' expr         { P.Binop $1 P.Geq $3 0 }
  | expr '>' expr          { P.Binop $1 P.Greater $3 0 }
  | expr '<=' expr         { P.Binop $1 P.Leq $3 0 }
  | expr '<' expr          { P.Binop $1 P.Less $3 0 }
  | expr '+' expr          { P.Binop $1 P.Add $3 0 }
  | expr '-' expr          { P.Binop $1 P.Sub $3 0 }
  | expr '*' expr          { P.Binop $1 P.Mul $3 0 }
  | expr '/' expr          { P.Binop $1 P.Div $3 0 }
  | expr '%' expr          { P.Binop $1 P.Mod $3 0 }
  | '(' expr ')'           { P.Nested $2 0  }
  | '!' expr               { P.Negate $2 0 }
  | '-' expr %prec NEG     { P.Negative $2 0  }
  | '&' expr %prec NEG     { P.AddressOf $2 0 }
  | '*' expr %prec NEG     { P.Deref $2 0 }
  | expr '.' ident         { P.FieldAccess $1 $3 0 }
  | expr '[' expr ']'      { P.ArrayAccess $1 $3 0 }
  | expr '->' ident        { P.Indirect $1 $3 0 }
  | sizeof '(' type array_sizes ')' { P.Sizeof (Left (P.SizeofType $3 $4)) 0 }
  | sizeof '(' expr ')'    { P.Sizeof (Right $3) 0 }
  | '(' type ')' expr %prec CAST { P.Typecast $2 $4 0}
  | ident '(' actuals ')'  { P.Call $1 (reverse $3) 0 }
  | ident '(' ')'          { P.Call $1 [] 0 }
  | expr '++'              { P.Increment $1 0 }
  | expr '--'              { P.Decrement $1 0 }
opt_expr:  {- empty -} { Nothing }
        | expr { Just $1 }

type: primitive_type stars { $1 $2 }
    | struct_type stars  { $1 $2 }
  
primitive_type: tint     { P.PrimitiveType L.Int    }
              | tdouble  { P.PrimitiveType L.Double }
              | tchar    { P.PrimitiveType L.Char   }
              | tvoid    { P.PrimitiveType L.Void   }

struct_type : struct ident { P.StructType $2 }
      
stars: {- empty -} { 0 }
     | stars '*' { $1 + 1 }

array_sizes: {- empty -} { [] }
     | array_sizes '[' int ']' { $3 : $1 }

actuals: expr { [$1] }
       | actuals ',' expr { $3 : $1 }

includes: {-empty-} { [] }
        | includes include string { (P.Include $3) : $1 }
{
parseError _ = error "Unable to parse tokens"
}