# mcc

> "clang done wrong"

**mcc** is a toy compiler compiling a (relatively) large subset of **C** language to **LLVM IR**. 

There are a few differences between the subset of **C** in specification of **C** language and the language being compiled by **mcc**. **mcc** compiles the subset which we call **mini C**.

**mini C** is an extension of the programming language used in [compiling techniques](https://www.inf.ed.ac.uk/teaching/courses/ct/19-20/) course by The University of Edinburgh.

Features of **mini C**:
- syntax exactly the same as **C**
- **mini C**'s primitive types are only **void, int, double, char**
- structs and arrays of arbitrary dimension are supported
- **mini C** passing struct by reference, passing struct by value

Key differences between **mini C** and **C** are:
- **mini C** has a built-in includes preprocessor which either resolves the compilation order of given source files or logs the presence of a cycle
- **mini C** does not support function pointers
- **mini C** does not support many implicit casts
- **mini C** does not support postfix operators such as ```i++```
- **mini C** does not support variable declaration and assignment in the same line
- **mini C** does not have formal and operational semantics :(

Lexical structure of **mini C** is equivalent to the lexical structure of the relevant subset of **C**.
Syntax of **mini C** is formally specified by the [grammar](./grammar). 

## Architecture
**mcc** follows the classical guidelines of compiler construction,
adapted to the functional programming paradigm.

### Lexical Analysis

Lexical Analysis is implemented in two ways. We have an **ad hoc** and **parser combinator** version.

We use **alex** generated lexer as a ground truth in unit testing, but it is not a part of compiler itself. 
### Includes Preprocessor (Topological Resolver)

Prior to the parsing stage, **mcc** constructs a dependency graph induced by includes and attempts to construct the **topological ordering**, reporting if such an ordering is impossible (i.e cyclic dependencies). The algorithm used is classic DFS.

### Syntax Analysis

The default method (method of choice) is a variant of **Pratt** top down operator precedence parser with error recovery.

Apart from **Pratt** parser, there are two more parsers implemented using **Megaparsec** combinator library. 

**Pratt** is implemented from scratch, delivering (probably) best error messages and overall the best error recovery.

We use **happy** generated parser (LR(k)) as a ground truth in unit testing, but it is not a part of compiler itself.

### Semantic Analysis

Classic tree walk typechecking, adapted to functional programming paradigm.

Apart from just typechecking, we perform:
- rewriting of pointer indirection (```->```) into a field access on dereferenced expression
- we rewrite all loops into do while to simplify code generation

### LLVM Code generation

Classic tree walk compilation, adapted to functional programming paradigm.
## Fun Facts

**mcc** is written entirely in Haskell and it started as a hobby project, done by @me and @its-sami. Most of this compiler has been **pair programmed**.

## Appendix - Syntax of mini C

```
program ::= (include)* (construct)* EOF

construct ::= structdecl
            | vardecl
            | funcdecl
            | funcdefn

include ::= Include LitString

structdecl ::= Struct Ident LBrace (vardecl)+ RBrace Semi

vardecl ::= type Ident [LBrack LitInt RBrack]* Semi

funcdecl ::= type Ident LParen formals RParen Semi
           | type Ident LParen RParen Semi

funcdefn ::= type Ident LParen formals RParen block

type ::= primitive_type
       | struct_type

primitive_type ::= (Type BuiltinType) stars
struct_type    ::= Struct Ident stars
sizeof_type    ::= type [Int]*

BuiltinType ::= Int
              | Double
              | Char
              | Void

stars ::= (Asterisk)*

formals ::= [ type Ident (Comma type Ident)* ]

stmt ::= expr Semi
       | block
       | While LParen expr RParen stmt
       | For LParen [expr] Semi [expr] Semi [expr] RParen stmt
       | If LParen expr RParen stmt
       | If LParen expr RParen stmt Else stmt
       | Return [expr] Semi
       | vardecl

block ::= LBrace [stmt]* RBrace

expr ::= LitInt
       | LitString
       | LitChar
       | LitDouble
       | LitNull
       | Ident
       | (expr)
       | expr op expr
       | Minus expr
       | Not expr
       | arrayaccess
       | fieldaccess
       | indirect
       | deref
       | addressof
       | funccall
       | assign
       | sizeof
       | typecast

op ::= Less
     | Leq
     | Greater
     | Geq
     | Neq
     | Equal
     | Plus
     | Minus
     | Div
     | Asterisk
     | Mod
     | Or
     | And
     | Caret
     | Ampers
     | Bar

funccall    ::= Ident LParen actuals RParen
              | Ident LParen RParen
arrayaccess ::= expr LBrack expr RBrack
fieldaccess ::= expr Dot Ident
indirect    ::= expr Arrow Ident
deref       ::= Asterisk expr
addressof   ::= Ampers expr
sizeof      ::= Sizeof LParen type RParen
              | Sizeof LParen expr RParen
typecast    ::= LParen type RParen expr
assign      ::= expr Assign expr

actuals ::= expr
          | actuals ',' expr
```