-- Main.hs
module Main where

import Lexer (lexTokens)
import Parser (runParseExpr, Expr(..))

main :: IO ()
main = do
    putStrLn "=== Probando MINILISP ==="
    
    -- Probar lexer
    putStrLn "Tokens:"
    print $ lexTokens "(+ 1 2)"
    
    -- Probar parser
    putStrLn "AST:"
    print $ runParseExpr "(+ 1 2)"
    
    putStrLn "¡Funciona! 🎉"