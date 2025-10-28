module Interpreter where

import Desugar (CoreExpr(..), desugar)
import Parser (runParseExpr)
import Data.Map (Map)
import qualified Data.Map as Map

-- =============================================================================
-- VALORES Y AMBIENTES
-- =============================================================================

type Env = Map String Value

data Value
  = NumV Int
  | BoolV Bool
  | ClosureV String CoreExpr Env
  deriving (Show)  -- QUITAMOS Eq

-- Ambiente vacío
emptyEnv :: Env
emptyEnv = Map.empty

-- Extender ambiente
extendEnv :: String -> Value -> Env -> Env
extendEnv x val env = Map.insert x val env

-- Buscar en ambiente
lookupEnv :: String -> Env -> Value
lookupEnv x env = case Map.lookup x env of
  Just val -> val
  Nothing  -> error $ "Variable libre: " ++ x

-- =============================================================================
-- INTERPRETE PRINCIPAL
-- =============================================================================

interp :: CoreExpr -> Env -> Value
interp (CId i) env = lookupEnv i env
interp (CNum n) env = NumV (fromInteger n)
interp (CBool b) env = BoolV b

-- Operadores aritméticos
interp (CAdd e1 e2) env =
  let v1 = interp e1 env
      v2 = interp e2 env
  in NumV (numVal v1 + numVal v2)

interp (CSub e1 e2) env =
  let v1 = interp e1 env
      v2 = interp e2 env
  in NumV (numVal v1 - numVal v2)

interp (CMul e1 e2) env =
  let v1 = interp e1 env
      v2 = interp e2 env
  in NumV (numVal v1 * numVal v2)

interp (CDiv e1 e2) env =
  let v1 = interp e1 env
      v2 = interp e2 env
  in if numVal v2 == 0 
     then error "División por cero"
     else NumV (numVal v1 `div` numVal v2)

-- Asignaciones locales
interp (CLet x e body) env =
  let val = interp e env
  in interp body (extendEnv x val env)

-- Condicionales
interp (CIf0 e1 e2 e3) env =
  let v1 = interp e1 env
  in if numVal v1 == 0 
     then interp e2 env 
     else interp e3 env

interp (CIf e1 e2 e3) env =
  let v1 = interp e1 env
  in if boolVal v1 
     then interp e2 env 
     else interp e3 env

-- Funciones y aplicaciones
interp (CFun x body) env = ClosureV x body env

interp (CApp func arg) env =
  let fval = interp func env
      aval = interp arg env
  in case fval of
      ClosureV x body closureEnv ->
        interp body (extendEnv x aval closureEnv)
      _ -> error "Aplicación de no-función"

-- =============================================================================
-- FUNCIONES AUXILIARES
-- =============================================================================

numVal :: Value -> Int
numVal (NumV n) = n
numVal _ = error "Se esperaba número"

boolVal :: Value -> Bool
boolVal (BoolV b) = b
boolVal _ = error "Se esperaba booleano"

-- =============================================================================
-- FUNCIONES DE PRUEBA
-- =============================================================================

-- Evaluar desde string (pipeline completo)
runProgram :: String -> Value
runProgram input = 
  let parsed = runParseExpr input
      desugared = desugar parsed
  in interp desugared emptyEnv

-- Mostrar resultados de forma legible
showValue :: Value -> String
showValue (NumV n) = show n
showValue (BoolV True) = "#t"
showValue (BoolV False) = "#f"
showValue (ClosureV x _ _) = "<fun:" ++ x ++ ">"

-- Probar ejemplos
testEval :: String -> IO ()
testEval input = do
  putStrLn $ ">>> " ++ input
  let result = runProgram input
  putStrLn $ "    " ++ showValue result
  putStrLn ""

-- Demostración
demo :: IO ()
demo = do
  putStrLn "=== INTERPRETE MINILISP ==="
  
  testEval "(+ 1 2)"
  testEval "(+ 1 2 3)"
  testEval "(let ((x 5)) (+ x 1))"
  testEval "((lambda (x) (+ x 1)) 5)"
  testEval "(if0 0 10 20)"
  testEval "(if #t 1 2)"
  
  putStrLn "¡Interprete funcionando! 🎉"