{
module Parser (parseProgram, parseExpr, Expr(..), runParseExpr) where

import Lexer (Token(..), lexTokens)
}

%tokentype { Token }

%token ident          { TokIdent $$ }
%token int            { TokInt $$ }
%token '#t'           { TokTrue }
%token '#f'           { TokFalse }
%token '('            { TokLP }
%token ')'            { TokRP }
%token '+'            { TokPlus }
%token '-'            { TokMinus }
%token '*'            { TokMul }
%token '/'            { TokDiv }
%token let            { TokLet }
%token if0            { TokIf0 }
%token if             { TokIf }
%token lambda         { TokLambda }

%name parseExpr Expr
%name parseProgram Program

%%

Program :: { Expr }
    : Expr { $1 }

Expr :: { Expr }
    : ident                  { Var $1 }
    | int                    { IntLit $1 }
    | '#t'                   { BoolLit True }
    | '#f'                   { BoolLit False }
    | '(' '+' Exprs ')'      { Add $3 }
    | '(' '-' Exprs ')'      { Sub $3 }
    | '(' '*' Exprs ')'      { Mul $3 }
    | '(' '/' Exprs ')'      { Div $3 }
    | '(' let '(' Bindings ')' Expr ')'     { Let $4 $6 }
    | '(' if0 Expr Expr Expr ')'    { If0 $3 $4 $5 }
    | '(' if Expr Expr Expr ')'     { If $3 $4 $5 }
    | '(' lambda '(' Idents ')' Expr ')'  { Lambda $4 $6 }
    | '(' Expr Exprs ')'              { App $2 $3 }

Exprs :: { [Expr] }
    : Expr Exprs      { $1 : $2 }
    | Expr            { [$1] }

Idents :: { [String] }
    : ident Idents    { $1 : $2 }
    | ident           { [$1] }

Bindings :: { [(String, Expr)] }
    : Binding Bindings   { $1 : $2 }
    | Binding            { [$1] }

Binding :: { (String, Expr) }
    : '(' ident Expr ')'   { ($2, $3) }

{
data Expr = Var String
          | IntLit Integer
          | BoolLit Bool
          | Add [Expr]
          | Sub [Expr]
          | Mul [Expr]
          | Div [Expr]
          | Let [(String, Expr)] Expr
          | If0 Expr Expr Expr
          | If Expr Expr Expr
          | Lambda [String] Expr
          | App Expr [Expr]
          deriving (Show, Eq)

-- Función para manejar errores de parsing (REQUERIDA por Happy)
happyError :: [Token] -> a
happyError tokens = error $ "Error de parsing en tokens: " ++ show tokens

-- Función auxiliar para parsing desde String
runParseExpr :: String -> Expr
runParseExpr = parseExpr . lexTokens
}