module Desugar where

import Parser (Expr(..))

-- Este es nuestro lenguaje "núcleo" mínimo.
-- Todo es unario o binario.
-- (NOTA: CLetRec ha sido eliminado, ahora es azúcar sintáctica)
data CoreExpr
  = CId String
  | CNum Integer
  | CBool Bool
  -- Operadores Binarios
  | CAdd CoreExpr CoreExpr
  | CSub CoreExpr CoreExpr
  | CMul CoreExpr CoreExpr
  | CDiv CoreExpr CoreExpr
  | CExpt CoreExpr CoreExpr -- Potencia
  | CSqrt CoreExpr       -- Raíz (unaria)
  -- Predicados Binarios
  | CEq CoreExpr CoreExpr
  | CNe CoreExpr CoreExpr
  | CLt CoreExpr CoreExpr
  | CGt CoreExpr CoreExpr
  | CLe CoreExpr CoreExpr
  | CGe CoreExpr CoreExpr
  | CNot CoreExpr        -- Not (unario)
  -- Asignaciones (Simples)
  | CLet String CoreExpr CoreExpr -- CLet es unario
  -- Condicional
  | CIf CoreExpr CoreExpr CoreExpr
  -- Funciones (Simples)
  | CFun String CoreExpr -- Lambda de 1 variable
  | CApp CoreExpr CoreExpr -- Aplicación de 1 argumento
  -- Pares
  | CPair CoreExpr CoreExpr
  | CFst CoreExpr
  | CSnd CoreExpr
  -- Listas
  | CNil
  | CCons CoreExpr CoreExpr
  | CHead CoreExpr
  | CTail CoreExpr
  deriving (Show, Eq)


-- (letrec ((f ...)) ...) se desazucara usando el combinador Y (de punto fijo).
-- Y_v = λf.(λx.f (λv.(x x) v)) (λx.f (λv.(x x) v))
-- Aquí lo construimos como un valor de CoreExpr:
yCombinator :: CoreExpr
yCombinator =
  CFun "f" (CApp lambdax lambdax)
  where
    xxv = CApp (CApp (CId "x") (CId "x")) (CId "v")
    lambdav = CFun "v" xxv
    inner = CApp (CId "f") lambdav
    lambdax = CFun "x" inner


desugar :: Expr -> CoreExpr
desugar (Var x) = CId x
desugar (IntLit n) = CNum n
desugar (BoolLit b) = CBool b

-- --- Operadores Variádicos -> Binarios Anidados ---
desugar (Add []) = CNum 0
desugar (Add [e]) = desugar e
desugar (Add (e:es)) = foldl CAdd (desugar e) (map desugar es)

desugar (Sub []) = error "Subtracción (-) requiere al menos un argumento"
desugar (Sub [e]) = CSub (CNum 0) (desugar e) -- (- e) -> 0 - e
desugar (Sub (e:es)) = foldl CSub (desugar e) (map desugar es)

desugar (Mul []) = CNum 1
desugar (Mul [e]) = desugar e
desugar (Mul (e:es)) = foldl CMul (desugar e) (map desugar es)

desugar (Div []) = error "División (/) requiere al menos un argumento"
desugar (Div [e]) = CDiv (CNum 1) (desugar e) -- (/ e) -> 1 / e
desugar (Div (e:es)) = foldl CDiv (desugar e) (map desugar es)

-- --- Azúcar Aritmética ---
desugar (Add1 e) = CAdd (desugar e) (CNum 1)
desugar (Sub1 e) = CSub (desugar e) (CNum 1)
desugar (Sqrt e) = CSqrt (desugar e)
desugar (Expt e1 e2) = CExpt (desugar e1) (desugar e2)

-- --- Predicados Variádicos -> Binarios Anidados ---
desugar (Eq []) = CBool True
desugar (Eq [e]) = CBool True -- (= e) siempre es verdadero
desugar (Eq (e:es)) = desugarPreds CEq (e:es)

desugar (Ne []) = CBool False
desugar (Ne [e]) = CBool False
desugar (Ne (e:es)) = desugarPreds CNe (e:es)

desugar (Lt []) = CBool True
desugar (Lt [e]) = CBool True
desugar (Lt (e:es)) = desugarChain CLt (e:es)

desugar (Gt []) = CBool True
desugar (Gt [e]) = CBool True
desugar (Gt (e:es)) = desugarChain CGt (e:es)

desugar (Le []) = CBool True
desugar (Le [e]) = CBool True
desugar (Le (e:es)) = desugarChain CLe (e:es)

desugar (Ge []) = CBool True
desugar (Ge [e]) = CBool True
desugar (Ge (e:es)) = desugarChain CGe (e:es)

desugar (Not e) = CNot (desugar e)

-- --- Asignaciones Múltiples -> Anidadas Simples ---

-- (let ((x e1) (y e2)) body) -> ((lambda (x y) body) e1 e2)
desugar (Let [] body) = desugar body
desugar (Let bindings body) =
  let vars = map fst bindings
      coreExprs = map (desugar . snd) bindings
      coreBody = desugar body
      -- Construye la lambda: (lambda (v1 v2 ...) body)
      lambda = foldr CFun coreBody vars
  -- Construye la aplicación: (lambda ... exprs ...)
  in foldl CApp lambda coreExprs

-- 'let*' (Secuencial) se desazucara en 'CLet' anidados
desugar (LetStar [] body) = desugar body
desugar (LetStar ((x, e):bindings) body) =
  CLet x (desugar e) (desugar (LetStar bindings body))

-- (letrec ((f e)) body) -> (let ((f (Y (lambda (f) e)))) body)
-- (Asumimos anidamiento para bindings múltiples)
desugar (LetRec [] body) = desugar body
desugar (LetRec ((x, e):bindings) body) =
  let -- 1. Crear (lambda (x) e)
      lambda = CFun x (desugar e)
      -- 2. Aplicar el combinador Y: (Y (lambda (x) e))
      yApplied = CApp yCombinator lambda
  -- 3. Enlazarlo con CLet y continuar: (let ((x (Y ...))) (letrec (...) ...))
  in CLet x yApplied (desugar (LetRec bindings body))

-- --- Condicionales ---
desugar (If0 e1 e2 e3) = CIf (CEq (desugar e1) (CNum 0)) (desugar e2) (desugar e3)
desugar (If e1 e2 e3) = CIf (desugar e1) (desugar e2) (desugar e3)

desugar (Cond (clauses, maybeElse)) =
  let elseExpr = case maybeElse of
                   Just e  -> desugar e
                   Nothing -> error "Cond debe tener una cláusula 'else'"
  in foldr (\(b, e) rest -> CIf (desugar b) (desugar e) rest) elseExpr clauses

-- --- Funciones Múltiples -> Currificadas Simples ---
desugar (Lambda [] body) = desugar body
desugar (Lambda (p:ps) body) =
  CFun p (desugar (Lambda ps body))

desugar (App func []) = desugar func
desugar (App func args) =
  foldl CApp (desugar func) (map desugar args)

-- --- Pares ---
desugar (Pair e1 e2) = CPair (desugar e1) (desugar e2)
desugar (Fst e) = CFst (desugar e)
desugar (Snd e) = CSnd (desugar e)

-- --- Listas ---
desugar (List []) = CNil
desugar (List (e:es)) = CCons (desugar e) (desugar (List es))
desugar (Head e) = CHead (desugar e)
desugar (Tail e) = CTail (desugar e)

-- Helper para (= 1 2 3) -> (&& (= 1 2) (= 2 3))
-- (Nota: esto es complejo y 'and' no está en el núcleo, así que lo desazucaramos a CIf)
desugarChain :: (CoreExpr -> CoreExpr -> CoreExpr) -> [Expr] -> CoreExpr
desugarChain op (e1:e2:es) =
  CIf (op (desugar e1) (desugar e2))
      (desugarChain op (e2:es))
      (CBool False)
desugarChain _ _ = CBool True -- Cadena de 1 o 0 elementos es verdadera

-- Helper para (= 1 1 1) -> (&& (= 1 1) (= 1 1))
-- Esto es más simple: todos se comparan con el primero.
desugarPreds :: (CoreExpr -> CoreExpr -> CoreExpr) -> [Expr] -> CoreExpr
desugarPreds op (e:es) =
  let v1 = desugar e
  in foldl (\b e2 -> CIf b (op v1 (desugar e2)) (CBool False)) (CBool True) es
desugarPreds _ [] = CBool True