{
module Parser (parseProgram, parseExpr, Expr(..)) where

import Lexer (Token(..))
}

-- Especificar el tipo de token
%tokentype { Token }

%token ident          { TokIdent $$ }
%token int            { TokInt $$ }
%token '#t'           { TokTrue }
%token '#f'           { TokFalse }
%token '('            { TokLP }
%token ')'            { TokRP }
%token '['            { TokLB }
%token ']'            { TokRB }
%token ','            { TokComma }
%token '+'            { TokPlus }
%token '-'            { TokMinus }
%token '*'            { TokMul }
%token '/'            { TokDiv }
%token add1           { TokAdd1 }
%token sub1           { TokSub1 }
%token sqrt           { TokSqrt }
%token expt           { TokExpt }
%token not            { TokNot }
%token '='            { TokEq }
%token '!='           { TokNe }
%token '<'            { TokLt }
%token '>'            { TokGt }
%token '<='           { TokLe }
%token '>='           { TokGe }
%token let            { TokLet }
%token 'let*'         { TokLetStar }
%token letrec         { TokLetRec }
%token if0            { TokIf0 }
%token if             { TokIf }
%token lambda         { TokLambda }
%token fst            { TokFst }
%token snd            { TokSnd }
%token head           { TokHead }
%token tail           { TokTail }
%token cond           { TokCond }
%token else           { TokElse }

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
    | '(' add1 Expr ')'      { Add1 $3 }
    | '(' sub1 Expr ')'      { Sub1 $3 }
    | '(' sqrt Expr ')'      { Sqrt $3 }
    | '(' expt Expr Expr ')' { Expt $3 $4 }
    | '(' not Expr ')'       { Not $3 }
    | '(' '=' Exprs ')'      { Eq $3 }
    | '(' '!=' Exprs ')'     { Ne $3 }
    | '(' '<' Exprs ')'      { Lt $3 }
    | '(' '>' Exprs ')'      { Gt $3 }
    | '(' '<=' Exprs ')'     { Le $3 }
    | '(' '>=' Exprs ')'     { Ge $3 }
    | '(' fst Expr ')'       { Fst $3 }
    | '(' snd Expr ')'       { Snd $3 }
    | '(' head Expr ')'      { Head $3 }
    | '(' tail Expr ')'      { Tail $3 }
    | '(' let '(' Bindings ')' Expr ')'     { Let $4 $6 }
    | '(' 'let*' '(' Bindings ')' Expr ')'  { LetStar $4 $6 }
    | '(' letrec '(' ident Expr ')' Expr ')' { LetRec $4 $5 $7 }
    | '(' if0 Expr Expr Expr ')'    { If0 $3 $4 $5 }
    | '(' if Expr Expr Expr ')'     { If $3 $4 $5 }
    | '(' lambda '(' Idents ')' Expr ')'  { Lambda $4 $6 }
    | '(' Expr Exprs ')'              { App $2 $3 }
    | '(' Expr ',' Expr ')'           { Pair $2 $4 }
    | '[' ListElements ']'            { List $2 }
    | '[' ']'                         { List [] }
    | '(' cond Clauses ')'            { Cond $3 }

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

ListElements :: { [Expr] }
    : Expr MoreElements { $1 : $2 }

MoreElements :: { [Expr] }
    : ',' Expr MoreElements { $2 : $3 }
    | {- empty -}           { [] }

Clauses :: { [(Expr, Expr)] }
    : Clause Clauses   { $1 : $2 }
    | ElseClause       { [$1] }

Clause :: { (Expr, Expr) }
    : '[' Expr Expr ']'   { ($2, $3) }

ElseClause :: { (Expr, Expr) }
    : '[' else Expr ']'   { (BoolLit True, $3) }

{
data Expr = Var String
          | IntLit Integer
          | BoolLit Bool
          | Add [Expr]
          | Sub [Expr]
          | Mul [Expr]
          | Div [Expr]
          | Add1 Expr
          | Sub1 Expr
          | Sqrt Expr
          | Expt Expr Expr
          | Not Expr
          | Eq [Expr]
          | Ne [Expr]
          | Lt [Expr]
          | Gt [Expr]
          | Le [Expr]
          | Ge [Expr]
          | Let [(String, Expr)] Expr
          | LetStar [(String, Expr)] Expr
          | LetRec String Expr Expr
          | If0 Expr Expr Expr
          | If Expr Expr Expr
          | Lambda [String] Expr
          | App Expr [Expr]
          | Pair Expr Expr
          | Fst Expr
          | Snd Expr
          | List [Expr]
          | Head Expr
          | Tail Expr
          | Cond [(Expr, Expr)] Expr
          deriving (Show, Eq)

-- Happy genera 'parse' automáticamente
parseProgram :: [Token] -> Expr
parseProgram tokens = case parse tokens of
  (expr, []) -> expr
  (_, rest) -> error $ "Tokens restantes: " ++ show rest

parseExpr :: [Token] -> Expr
parseExpr tokens = case parseExpr tokens of
  (expr, []) -> expr
  (_, rest) -> error $ "Tokens restantes: " ++ show rest
}