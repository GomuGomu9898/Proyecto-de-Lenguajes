module Desugar where

import Parser (Expr(..), runParseExpr)

-- Núcleo mínimo - solo con constructores básicos
data CoreExpr
  = CId String
  | CNum Integer
  | CBool Bool
  | CAdd CoreExpr CoreExpr
  | CSub CoreExpr CoreExpr
  | CMul CoreExpr CoreExpr
  | CDiv CoreExpr CoreExpr
  | CLet String CoreExpr CoreExpr
  | CIf0 CoreExpr CoreExpr CoreExpr
  | CIf CoreExpr CoreExpr CoreExpr
  | CFun String CoreExpr
  | CApp CoreExpr CoreExpr
  deriving (Show)

-- Desazucarización SOLO para los constructores que existen
desugar :: Expr -> CoreExpr
desugar (Var x) = CId x
desugar (IntLit n) = CNum n
desugar (BoolLit b) = CBool b

-- Operadores variádicos -> binarios
desugar (Add []) = error "Add sin argumentos"
desugar (Add [e]) = desugar e
desugar (Add (e:es)) = foldl1 CAdd (map desugar (e:es))

desugar (Sub []) = error "Sub sin argumentos"
desugar (Sub [e]) = CSub (CNum 0) (desugar e)
desugar (Sub (e:es)) = foldl1 CSub (map desugar (e:es))

desugar (Mul []) = error "Mul sin argumentos"
desugar (Mul [e]) = desugar e
desugar (Mul (e:es)) = foldl1 CMul (map desugar (e:es))

desugar (Div []) = error "Div sin argumentos"
desugar (Div [e]) = CDiv (CNum 1) (desugar e)
desugar (Div (e:es)) = foldl1 CDiv (map desugar (e:es))

-- Let múltiple -> anidado
desugar (Let [] body) = desugar body
desugar (Let ((x, e):bindings) body) = 
  CLet x (desugar e) (desugar (Let bindings body))

-- Condicionales directos
desugar (If0 e1 e2 e3) = CIf0 (desugar e1) (desugar e2) (desugar e3)
desugar (If e1 e2 e3) = CIf (desugar e1) (desugar e2) (desugar e3)

-- Lambda múltiple -> currificada
desugar (Lambda [] body) = desugar body
desugar (Lambda (p:ps) body) = 
  CFun p (desugar (Lambda ps body))

-- Aplicación múltiple -> anidada
desugar (App func []) = desugar func
desugar (App func (arg:args)) =
  CApp (desugar (App func [arg])) (desugarArgs args)
  where
    desugar (App func [arg]) = CApp (desugar func) (desugar arg)
    desugarArgs [] = error "No debería pasar"
    desugarArgs [a] = desugar a
    desugarArgs (a:as) = CApp (desugarArgs [a]) (desugarArgs as)

-- Función para pruebas
testDesugar :: String -> CoreExpr
testDesugar input = case runParseExpr input of
  expr -> desugar expr