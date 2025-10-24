{
-- Cabecera Haskell (se copia literal en el .hs generado)
module Lexer where

import Data.Char (isAlpha, isDigit, isAlphaNum)
}

-- wrapper básico de Alex
%wrapper "basic"

-- Definiciones de patrones
$digit = [0-9]
$alpha = [a-zA-Z]
$alphanum = [a-zA-Z0-9]
$whitespace = [\ \t\n\r]

-- Tokens que reconoceremos
tokens :-

  -- Espacios en blanco (se ignoran)
  $whitespace+ ;

  -- Comentarios (línea que empieza con ;)
  ";".* ;

  -- Booleanos
  \#t    { \s -> TokenBool True }
  \#f    { \s -> TokenBool False }

  -- Palabras reservadas
  "let"    { \s -> TokenLet }
  "let*"   { \s -> TokenLetStar }
  "letrec" { \s -> TokenLetRec }
  "lambda" { \s -> TokenLambda }
  "if"     { \s -> TokenIf }
  "if0"    { \s -> TokenIf0 }
  "cond"   { \s -> TokenCond }
  "else"   { \s -> TokenElse }
  "head"   { \s -> TokenHead }
  "tail"   { \s -> TokenTail }
  "fst"    { \s -> TokenFst }
  "snd"    { \s -> TokenSnd }
  "not"    { \s -> TokenNot }
  "add1"   { \s -> TokenAdd1 }
  "sub1"   { \s -> TokenSub1 }
  "sqrt"   { \s -> TokenSqrt }
  "expt"   { \s -> TokenExpt }

  -- Operadores aritméticos
  "+"      { \s -> TokenAdd }
  "-"      { \s -> TokenSub }
  "*"      { \s -> TokenMul }
  "/"      { \s -> TokenDiv }

  -- Operadores de comparación
  "="      { \s -> TokenEq }
  "<"      { \s -> TokenLt }
  ">"      { \s -> TokenGt }
  ">="     { \s -> TokenGe }
  "<="     { \s -> TokenLe }
  "!="     { \s -> TokenNe }

  -- Delimitadores
  "("      { \s -> TokenLParen }
  ")"      { \s -> TokenRParen }
  "["      { \s -> TokenLBracket }
  "]"      { \s -> TokenRBracket }
  ","      { \s -> TokenComma }

  -- Números enteros
  $digit+  { \s -> TokenInt (read s) }

  -- Identificadores/variables
  $alpha [$alphanum \-\?]* { \s -> TokenVar s }

{
-- Código Haskell adicional (se incluye en el .hs generado)

-- Tipo de datos para los tokens
data Token
    = TokenInt Int
    | TokenBool Bool
    | TokenVar String
    | TokenLet
    | TokenLetStar
    | TokenLetRec
    | TokenLambda
    | TokenIf
    | TokenIf0
    | TokenCond
    | TokenElse
    | TokenHead
    | TokenTail
    | TokenFst
    | TokenSnd
    | TokenNot
    | TokenAdd1
    | TokenSub1
    | TokenSqrt
    | TokenExpt
    | TokenAdd
    | TokenSub
    | TokenMul
    | TokenDiv
    | TokenEq
    | TokenLt
    | TokenGt
    | TokenGe
    | TokenLe
    | TokenNe
    | TokenLParen
    | TokenRParen
    | TokenLBracket
    | TokenRBracket
    | TokenComma
    | TokenEOF
    deriving (Show, Eq)

-- Función principal que convierte String en lista de Tokens
alexScanTokens :: String -> [Token]
alexScanTokens input = 
    case alexScan input 0 of
        AlexEOF -> []
        AlexError _ -> error "Error léxico"
        AlexSkip inp' len -> alexScanTokens inp'
        AlexToken inp' len action -> action (take len input) : alexScanTokens inp'
}