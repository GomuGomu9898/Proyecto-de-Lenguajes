{
module Parser (parseProgram, parseExpr, Expr(..), runParseExpr) where

import Lexer (Token(..), lexTokens)
}

%tokentype { Token }

-- Tokens Atómicos
%token ident          { TokIdent $$ }
%token int            { TokInt $$ }
%token '#t'           { TokTrue }
%token '#f'           { TokFalse }

-- Delimitadores
%token '('            { TokLP }
%token ')'            { TokRP }
%token '['            { TokLB }
%token ']'            { TokRB }
%token ','            { TokComma }

-- Palabras Clave
%token lambda         { TokLambda }
%token if             { TokIf }
%token if0            { TokIf0 }
%token let            { TokLet }
%token letrec         { TokLetRec }
%token letstar { TokLetStar } -- <<< CHANGED NAME HERE
%token cond           { TokCond }
%token else           { TokElse }

-- Pares y Listas
%token fst            { TokFst }
%token snd            { TokSnd }
%token head           { TokHead }
%token tail           { TokTail }

-- Operadores
%token '+'            { TokPlus }
%token '-'            { TokMinus }
%token '*'            { TokMul }
%token '/'            { TokDiv }
%token add1           { TokAdd1 }
%token sub1           { TokSub1 }
%token sqrt           { TokSqrt }
%token expt           { TokExpt }
%token '='            { TokEq }
%token '!='           { TokNe }
%token '<='           { TokLe }
%token '>='           { TokGe }
%token '<'            { TokLt }
%token '>'            { TokGt }
%token not            { TokNot }
-- 'and' y 'or' se añaden como desafío

%name parseExpr Expr
%name parseProgram Program

%%

Program :: { Expr }
    : Expr { $1 }

Expr :: { Expr }
    -- Atómicas
    : ident                  { Var $1 }
    | int                    { IntLit $1 }
    | '#t'                   { BoolLit True }
    | '#f'                   { BoolLit False }

    -- Listas
    | '[' ']'                { List [] }
    | '[' ExprsList ']'      { List $2 }

    -- Expresiones-S
    | '(' ')'                { error "Expresión () vacía no es válida" }
    | '(' ExprInner ')'      { $2 }

ExprInner :: { Expr }
    -- Aritmética
    : '+' Exprs              { Add $2 }
    | '-' Exprs              { Sub $2 }
    | '*' Exprs              { Mul $2 }
    | '/' Exprs              { Div $2 }
    | add1 Expr              { Add1 $2 }
    | sub1 Expr              { Sub1 $2 }
    | sqrt Expr              { Sqrt $2 }
    | expt Expr Expr         { Expt $2 $3 }

    -- Predicados
    |'=' Exprs              { Eq $2 }
    | '!=' Exprs             { Ne $2 }
    | '<' Exprs              { Lt $2 }
    | '>' Exprs              { Gt $2 }
    | '<=' Exprs             { Le $2 }
    | '>=' Exprs             { Ge $2 }
    | not Expr               { Not $2 }

    -- Asignaciones
    | let '(' Bindings ')' Expr     { Let $3 $5 }
    | letstar '(' Bindings ')' Expr { LetStar $3 $5 } -- <<< CHANGED NAME HERE
    | letrec '(' Bindings ')' Expr  { LetRec $3 $5 }

    -- Condicionales
    | if0 Expr Expr Expr     { If0 $2 $3 $4 }
    | if Expr Expr Expr      { If $2 $3 $4 }
    | cond Clauses           { Cond $2 }

    -- Pares
    | ',' Expr Expr          { Pair $2 $3 } -- Sintaxis (e1, e2)
    | fst Expr               { Fst $2 }
    | snd Expr               { Snd $2 }

    -- Listas
    | head Expr              { Head $2 }
    | tail Expr              { Tail $2 }

    -- Funciones
    | lambda '(' Idents ')' Expr  { Lambda $3 $5 }
    | Expr Exprs                  { App $1 $2 }

-- Reglas Auxiliares
-- ... (resto de las reglas auxiliares sin cambios) ...

Exprs :: { [Expr] }
    : Expr Exprs      { $1 : $2 }
    | Expr            { [$1] }

ExprsList :: { [Expr] }
    : Expr              { [$1] }
    | Expr ',' ExprsList  { $1 : $3 }

Idents :: { [String] }
    : ident Idents    { $1 : $2 }
    | ident           { [$1] }

Bindings :: { [(String, Expr)] }
    : Binding Bindings   { $1 : $2 }
    | Binding            { [$1] }

Binding :: { (String, Expr) }
    : '(' ident Expr ')'   { ($2, $3) }

Clauses :: { ([(Expr, Expr)], Maybe Expr) }
    : Clause Clauses                    { let (cs, e) = $2 in (($1 : cs), e) }
    | '(' else Expr ')'                 { ([], Just $3) }
    | Clause                            { ([$1], Nothing) }

Clause :: { (Expr, Expr) }
    : '(' Expr Expr ')'        { ($2, $3) }


{
-- ESTA ES LA SINTAXIS DE SUPERFICIE (Surface ASA)
-- ... (definición del tipo Expr sin cambios) ...
data Expr = Var String
          | IntLit Integer
          | BoolLit Bool
          -- Operadores
          | Add [Expr]
          | Sub [Expr]
          | Mul [Expr]
          | Div [Expr]
          | Add1 Expr
          | Sub1 Expr
          | Sqrt Expr
          | Expt Expr Expr
          -- Predicados
          | Eq [Expr]
          | Ne [Expr]
          | Lt [Expr]
          | Gt [Expr]
          | Le [Expr]
          | Ge [Expr]
          | Not Expr
          -- Asignaciones
          | Let [(String, Expr)] Expr
          | LetStar [(String, Expr)] Expr -- Constructor sigue igual
          | LetRec [(String, Expr)] Expr
          -- Condicionales
          | If0 Expr Expr Expr
          | If Expr Expr Expr
          | Cond ([(Expr, Expr)], Maybe Expr)
          -- Funciones
          | Lambda [String] Expr
          | App Expr [Expr]
          -- Pares
          | Pair Expr Expr
          | Fst Expr
          | Snd Expr
          -- Listas
          | List [Expr]
          | Head Expr
          | Tail Expr
          deriving (Show, Eq)


-- Función para manejar errores de parsing (REQUERIDA por Happy)
happyError :: [Token] -> a
happyError tokens = error $ "Error de parsing en tokens: " ++ show tokens

-- Función auxiliar para parsing desde String
runParseExpr :: String -> Expr
runParseExpr = parseExpr . lexTokens

}