{
module Lexer (Token(..), lexTokens) where
import Data.Char (ord)
import Data.Array (Array, listArray, (!), bounds)
}

%wrapper "basic"

-- Blancos robustos: espacio, tab, LF, CR, FF, VT
$ws      = [\x20\x09\x0A\x0D\x0C\x0B]
$digit   = 0-9
$alpha   = [A-Za-z_]
$idrest  = [A-Za-z0-9_\-\?\!]

-- literales útiles
@int     = \-?$digit+
@ident   = $alpha$idrest*

tokens :-

-- Ignorar espacios y comentarios tipo Lisp: ; ... hasta fin de línea
$ws+                         ;
";" [^\x0A\x0D]*             ;

-- Delimitadores y separadores
"("                          { \_ -> TokLP     }
")"                          { \_ -> TokRP     }
"["                          { \_ -> TokLB     }
"]"                          { \_ -> TokRB     }
","                          { \_ -> TokComma  }

-- Palabras clave / especiales
"lambda"                     { \_ -> TokLambda }
"if"                         { \_ -> TokIf     }
"if0"                        { \_ -> TokIf0    }
"let"                        { \_ -> TokLet    }
"letrec"                     { \_ -> TokLetRec }
"let*"                       { \_ -> TokLetStar}
"cond"                       { \_ -> TokCond   }
"else"                       { \_ -> TokElse   }
"fst"                        { \_ -> TokFst    }
"snd"                        { \_ -> TokSnd    }
"head"                       { \_ -> TokHead   }
"tail"                       { \_ -> TokTail   }

-- Operadores
"+"                          { \_ -> TokPlus   }
"-"                          { \_ -> TokMinus  }
"*"                          { \_ -> TokMul    }
"/"                          { \_ -> TokDiv    }
"add1"                       { \_ -> TokAdd1   }
"sub1"                       { \_ -> TokSub1   }
"sqrt"                       { \_ -> TokSqrt   }
"expt"                       { \_ -> TokExpt   }
"="                          { \_ -> TokEq     }
"!="                         { \_ -> TokNe     }
"<="                         { \_ -> TokLe     }
">="                         { \_ -> TokGe     }
"<"                          { \_ -> TokLt     }
">"                          { \_ -> TokGt     }
"not"                        { \_ -> TokNot    }

-- Booleanos
"#t"                         { \_ -> TokTrue   }
"#f"                         { \_ -> TokFalse  }

-- Literales e identificadores
@int                         { \s -> TokInt   (read s) }
@ident                       { \s -> TokIdent s       }

-- Cualquier otro símbolo: error léxico
.                            { \s -> lexPanic s }

{
-- === Definición de tokens (nombres distintos a tu ejemplo) ===
data Token
  = TokLP | TokRP | TokLB | TokRB | TokComma
  | TokLambda | TokIf | TokIf0
  | TokLet | TokLetRec | TokLetStar | TokCond | TokElse
  | TokFst | TokSnd | TokHead | TokTail
  | TokPlus | TokMinus | TokMul | TokDiv
  | TokAdd1 | TokSub1 | TokSqrt | TokExpt
  | TokEq | TokLt | TokGt | TokLe | TokGe | TokNe | TokNot
  | TokTrue | TokFalse
  | TokInt Integer
  | TokIdent String
  deriving (Eq, Show)

-- Alex expone alexScanTokens :: String -> [Token]
lexTokens :: String -> [Token]
lexTokens = alexScanTokens

lexPanic :: String -> a
lexPanic s =
  error $ "Lexer: carácter no reconocido: " ++ show s
       ++ " (codepoints=" ++ show (map ord s) ++ ")"
}
