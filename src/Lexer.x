{
module Lexer (lexTokens) where
import Tokens
}

%wrapper "basic"

$white   = [\x20\x09\x0D\x0A]+       -- espacio, tab, CR, LF
$digit   = [0-9]
$idStart = [A-Za-z_]
$idCont  = [A-Za-z0-9_\-\?\!]

@int    = \-?$digit+
@ident  = $idStart$idCont*

tokens :-

$white                       ;
"("                          { \_ -> TLParen }
")"                          { \_ -> TRParen }
"["                          { \_ -> TLBracket }
"]"                          { \_ -> TRBracket }
","                          { \_ -> TComma }

"lambda"                     { \_ -> TLambda }
"if"                         { \_ -> TIf }
"if0"                        { \_ -> TIf0 }
"letrec"                     { \_ -> TLetRec }
"let*"                       { \_ -> TLetStar }
"let"                        { \_ -> TLet }
"cond"                       { \_ -> TCond }
"else"                       { \_ -> TElse }
"fst"                        { \_ -> TFst }
"snd"                        { \_ -> TSnd }
"head"                       { \_ -> THead }
"tail"                       { \_ -> TTail }

"+"                          { \_ -> TOpPlus }
"-"                          { \_ -> TOpMinus }
"*"                          { \_ -> TOpMul }
"/"                          { \_ -> TOpDiv }
"add1"                       { \_ -> TOpAdd1 }
"sub1"                       { \_ -> TOpSub1 }
"sqrt"                       { \_ -> TOpSqrt }
"expt"                       { \_ -> TOpExpt }
"="                          { \_ -> TEq }
"!="                         { \_ -> TNe }
"<="                         { \_ -> TLe }
">="                         { \_ -> TGe }
"<"                          { \_ -> TLt }
">"                          { \_ -> TGt }
"not"                        { \_ -> TNot }

"#t"                         { \_ -> TTrue }
"#f"                         { \_ -> TFalse }

@int                         { \s -> TInt (read s) }
@ident                       { \s -> TIdent s }

{
lexTokens :: String -> [Token]
lexTokens = alexScanTokens
}
