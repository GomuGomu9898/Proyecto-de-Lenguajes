{
module Grammars (parse, Exp(..), runParse) where

import Lex (Token(..), lexer)
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  VAR     { TokenId $$ }
  INT     { TokenNum $$ }
  BOOL    { TokenBool $$ }
  PLUS    { TokenSuma }
  MINUS   { TokenResta }
  NOTKW   { TokenNot }
  LP      { TokenPA }
  RP      { TokenPC }
  LETKW   { TokenLet }
  LAMBD   { TokenLambda }
%%
-- Expresión principal (similar a tu SASA, pero con otros nombres)
Expr
  : Atom                               { $1 }
  | LP PLUS  Expr Expr RP              { EAdd $3 $4 }
  | LP MINUS Expr Expr RP              { ESub $3 $4 }
  | LP NOTKW Expr RP                   { ENot $3 }
  | LP LETKW LP VAR Expr RP Expr RP    { ELet $4 $5 $7 }
  | LP LAMBD LP VAR RP Expr RP         { ELam $4 $6 }
  | LP Expr Expr RP                    { EApp $2 $3 }
  ;

-- Átomos
Atom
  : VAR                                 { EVar $1 }
  | INT                                 { ENum $1 }
  | BOOL                                { EBool $1 }
  ;
%%
{
-- AST distinto al tuyo, pero con la misma semántica
data Exp
  = EVar String
  | ENum Int
  | EBool Bool
  | EAdd Exp Exp
  | ESub Exp Exp
  | ENot Exp
  | ELet String Exp Exp
  | ELam String Exp
  | EApp Exp Exp
  deriving (Show, Eq)

-- Happy moderno: recibe UN Token, no [Token]
parseError :: Token -> a
parseError t = error ("Parse error (token inesperado): " ++ show t)

-- Helper para probar directo con tu lexer
runParse :: String -> Exp
runParse = parse . lexer
}
