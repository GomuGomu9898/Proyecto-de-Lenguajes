module Interpreter where

import Desugar (CoreExpr(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- =============================================================================
-- VALORES Y AMBIENTES
-- =============================================================================

type Env = Map String Value

-- Valores que nuestro lenguaje puede manejar
data Value
  = NumV Integer
  | BoolV Bool
  | ClosureV String CoreExpr Env
  | PairV Value Value
  | ListV [Value]
  | ErrorV String -- Para errores

-- Instancia de Show para imprimir valores
instance Show Value where
  show (NumV n) = show n
  show (BoolV True) = "#t"
  show (BoolV False) = "#f"
  show (ClosureV x _ _) = "<fun:" ++ x ++ ">"
  show (PairV v1 v2) = "(" ++ show v1 ++ " , " ++ show v2 ++ ")"
  -- Hacemos que la lista se imprima como en el input
  show (ListV vs) = "[" ++ showList vs ++ "]"
    where
      showList [] = ""
      showList [v] = show v
      showList (v:vs') = show v ++ ", " ++ showList vs'
  show (ErrorV s) = "ERROR: " ++ s

-- Ambiente vacío
emptyEnv :: Env
emptyEnv = Map.empty

-- Extender ambiente
extendEnv :: String -> Value -> Env -> Env
extendEnv = Map.insert

-- Buscar en ambiente
lookupEnv :: String -> Env -> Value
lookupEnv x env = fromMaybe (ErrorV ("Variable libre: " ++ x)) (Map.lookup x env)

-- =============================================================================
-- INTERPRETE PRINCIPAL
-- =============================================================================

interp :: CoreExpr -> Env -> Value
interp (CId i) env = lookupEnv i env
interp (CNum n) _ = NumV n
interp (CBool b) _ = BoolV b

-- --- Aritmética ---
interp (CAdd e1 e2) env = numOp (+) (interp e1 env) (interp e2 env)
interp (CSub e1 e2) env = numOp (-) (interp e1 env) (interp e2 env)
interp (CMul e1 e2) env = numOp (*) (interp e1 env) (interp e2 env)
interp (CDiv e1 e2) env =
  let v1 = interp e1 env
      v2 = interp e2 env
  in case (v1, v2) of
       (NumV n1, NumV 0) -> ErrorV "División por cero"
       (NumV n1, NumV n2) -> NumV (n1 `div` n2)
       _ -> ErrorV "División requiere números"
       
interp (CSqrt e) env =
  case (interp e env) of
    (NumV n) -> if n < 0 then ErrorV "Raíz de número negativo" else NumV (floor (sqrt (fromInteger n :: Double)))
    _ -> ErrorV "Sqrt requiere número"

interp (CExpt e1 e2) env =
  let v1 = interp e1 env
      v2 = interp e2 env
  in case (v1, v2) of
       (NumV n1, NumV n2) -> NumV (n1 ^ n2)
       _ -> ErrorV "Expt requiere números"

-- --- Predicados ---
interp (CEq e1 e2) env = valEq (interp e1 env) (interp e2 env)
interp (CNe e1 e2) env = case valEq (interp e1 env) (interp e2 env) of
                          BoolV b -> BoolV (not b)
                          err -> err
interp (CLt e1 e2) env = numCmp (<) (interp e1 env) (interp e2 env)
interp (CGt e1 e2) env = numCmp (>) (interp e1 env) (interp e2 env)
interp (CLe e1 e2) env = numCmp (<=) (interp e1 env) (interp e2 env)
interp (CGe e1 e2) env = numCmp (>=) (interp e1 env) (interp e2 env)
interp (CNot e) env = boolOp not (interp e env)

-- --- Condicional ---
interp (CIf e1 e2 e3) env =
  case (interp e1 env) of
    (BoolV True)  -> interp e2 env
    (BoolV False) -> interp e3 env
    (ErrorV s)    -> ErrorV s -- Propagar error de la condición
    _             -> ErrorV "Condición de 'if' no es booleana"

-- --- Asignaciones ---
interp (CLet x e body) env =
  let val = interp e env
  in interp body (extendEnv x val env)

-- 'CLetRec' ya no es una primitiva.
-- El 'Desugarer' (Desugar.hs) que te di lo maneja con el Combinador Y,
-- convirtiéndolo en 'CLet', 'CApp' y 'CFun'.
-- Por lo tanto, esta regla ya no es necesaria y ha sido eliminada.

-- --- Funciones ---
interp (CFun x body) env = ClosureV x body env
interp (CApp func arg) env =
  let fval = interp func env
      aval = interp arg env
  in case fval of
      ClosureV x body closureEnv ->
        interp body (extendEnv x aval closureEnv)
      (ErrorV s) -> ErrorV s -- Propagar error de la función
      _ -> ErrorV "Aplicación de no-función"

-- --- Pares ---
interp (CPair e1 e2) env = PairV (interp e1 env) (interp e2 env)
interp (CFst e) env =
  case (interp e env) of
    (PairV v1 _) -> v1
    _            -> ErrorV "fst aplicado a no-par"
interp (CSnd e) env =
  case (interp e env) of
    (PairV _ v2) -> v2
    _            -> ErrorV "snd aplicado a no-par"

-- --- Listas ---
interp CNil _ = ListV []
interp (CCons e_head e_tail) env =
  let v_head = interp e_head env
      v_tail = interp e_tail env
  in case v_tail of
      (ListV vs) -> ListV (v_head : vs)
      _          -> ErrorV "cons aplicado a no-lista"

interp (CHead e) env =
  case (interp e env) of
    (ListV (h:_)) -> h
    (ListV [])    -> ErrorV "head aplicado a lista vacía"
    _             -> ErrorV "head aplicado a no-lista"

interp (CTail e) env =
  case (interp e env) of
    (ListV (_:t)) -> ListV t
    (ListV [])    -> ErrorV "tail aplicado a lista vacía"
    _             -> ErrorV "tail aplicado a no-lista"

-- =============================================================================
-- FUNCIONES AUXILIARES DE EVALUACIÓN
-- =============================================================================

numOp :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
numOp op (NumV n1) (NumV n2) = NumV (op n1 n2)
numOp _ _ _ = ErrorV "Operación aritmética requiere números"

numCmp :: (Integer -> Integer -> Bool) -> Value -> Value -> Value
numCmp op (NumV n1) (NumV n2) = BoolV (op n1 n2)
numCmp _ _ _ = ErrorV "Comparación requiere números"

boolOp :: (Bool -> Bool) -> Value -> Value
boolOp op (BoolV b) = BoolV (op b)
boolOp _ _ = ErrorV "Operación lógica requiere booleanos"

-- Igualdad profunda (Corregida)
valEq :: Value -> Value -> Value
valEq (NumV n1) (NumV n2) = BoolV (n1 == n2)
valEq (BoolV b1) (BoolV b2) = BoolV (b1 == b2)
valEq (ListV []) (ListV []) = BoolV True
valEq (ListV (h1:t1)) (ListV (h2:t2)) =
  case valEq h1 h2 of
    (BoolV True)  -> valEq (ListV t1) (ListV t2)
    (BoolV False) -> BoolV False
    err           -> err
valEq (PairV a1 b1) (PairV a2 b2) =
  case valEq a1 a2 of
    (BoolV True)  -> valEq b1 b2
    (BoolV False) -> BoolV False
    err           -> err
-- La igualdad de funciones no está definida
valEq (ClosureV {}) (ClosureV {}) = ErrorV "No se pueden comparar funciones"
-- Comparar cualquier otra cosa de tipos diferentes da Falso
valEq _ _ = BoolV False