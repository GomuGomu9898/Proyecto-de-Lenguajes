-- Main.hs
module Main where

import Lexer (lexTokens)
import Parser (runParseExpr, Expr(..))
import Desugar (desugar, CoreExpr(..))
import Interpreter (interp, emptyEnv, Value(..))

import System.IO (stdout, hFlush, hSetBuffering, BufferMode(NoBuffering))
import Control.Exception (try, evaluate, SomeException)

-- Esta función toma un string, lo procesa por todo el pipeline y devuelve
-- un valor final. Usamos 'evaluate' para forzar la evaluación dentro del 'try'.
runProgram :: String -> IO Value
runProgram input = do
  let surfaceAst = runParseExpr input
      coreAst    = desugar surfaceAst
  evaluate (interp coreAst emptyEnv)

factorial :: String
factorial = "(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (sub1 n))))))) (fact 5))"

fibonacci :: String
fibonacci = "(letrec ((fib (lambda (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2))))))) (fib 7))"

sumN :: String
sumN = "(letrec ((sum (lambda (n) (if (= n 0) 0 (+ n (sum (sub1 n))))))) (sum 10))"

-- Lista de todos los tests
requiredTests :: [(String, String)]
requiredTests =
  [ ("Factorial(5)", factorial)
  , ("Fibonacci(7)", fibonacci)
  , ("Suma(1..10)", sumN)
  ]


-- Función para correr un solo test y mostrar el resultado
testEval :: String -> String -> IO ()
testEval name input = do
  putStrLn $ "--- Test: " ++ name ++ " ---"
  putStrLn $ ">>> " ++ input
  runAndPrint input
  putStrLn ""

-- Función helper para correr todos los tests
runAllTests :: IO ()
runAllTests = mapM_ (uncurry testEval) requiredTests

-- Función que toma un string, lo evalúa y maneja errores
runAndPrint :: String -> IO ()
runAndPrint input = do
  -- Usamos 'try' para atrapar errores de parsing o de runtime (ej. div por cero)
  result <- try (runProgram input) :: IO (Either SomeException Value)
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right val -> print val

-- El loop principal del REPL
repl :: IO ()
repl = do
  putStr "minilisp> "
  hFlush stdout -- Asegura que "minilisp> " se muestre antes de getLine
  line <- getLine

  case line of
    ":q" -> putStrLn "Adiós."
    ":test" -> do
      runAllTests
      repl
    "" -> repl -- Ignora líneas vacías
    _  -> do
      runAndPrint line
      repl

-- Función Main: inicializa el REPL
main :: IO ()
main = do
  -- Desactiva el buffering para que el REPL se sienta interactivo
  hSetBuffering stdout NoBuffering
  putStrLn "=== Intérprete MINILISP (REPL) ==="
  putStrLn "Escribe una expresión, o usa :test para correr las pruebas, :q para salir."
  repl