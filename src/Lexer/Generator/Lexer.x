{
module Lexer.Generator.Lexer where
import Lexer.Lexeme
}

%wrapper "basic"

$alpha = [a-zA-Z]
$digit = 0-9
$newline = [\r\n]

tokens :-
 $white+  ;
 "/*" ( $newline | [^\*] | \*+ ($newline | [^\/]) )* "*/" ;
 "//" [^$newline]* $newline ;

 \(         { const LParen   }
 \)         { const RParen   }
 \{         { const LBrace   }
 \}         { const RBrace   }
 \[         { const LBrack   }
 \]         { const RBrack   }
 \;         { const Semi     }
 \,         { const Comma    }
 \+         { const Plus     }
 \+\+       { const Increment }
 \-         { const Minus    }
 \-\-       { const Decrement }
 \*         { const Asterisk }
 \/         { const Div      }
 \=         { const Assign   }
 \=\=       { const Equal    }
 \!\=       { const Neq      }
 \<         { const Less     }
 \<\=       { const Leq      }
 \>         { const Greater  }
 \>\=       { const Geq      }
 \&\&       { const And      }
 \|\|       { const Or       }
 \!         { const Not      }
 \&         { const Ampers   }
 \%         { const Mod      }
 \ˆ         { const Caret    }
 \|         { const Bar      }
 \.         { const Dot      }
 \-\>       { const Arrow    }
 "if"       { const If       }
 "else"     { const Else     }
 "for"      { const For      }
 "while"    { const While    }
 "return"   { const Return   }
 "struct"   { const Struct   }
 "sizeof"   { const Sizeof   }
 "#include" { const Include  }

 "int"      { const $ Type Int    }
 "double"   { const $ Type Double }
 "char"     { const $ Type Char   }
 "void"     { const $ Type Void   }

 "NULL"                     { const LitNull }
 $digit+                    { LitInt . read }
 $digit+ \. $digit*         { LitDouble . read }
 $alpha [$alpha $digit \_]* { Ident }
 \"(\\.|[^\\\"])*\"         { LitString . read }
 \' (\\.|[^\\\']) \'        { LitChar . read }

 . { const Error }

{
lex' :: String -> [Lexeme]
lex' input = alexScanTokens input ++ [Eof]
}