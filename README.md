# Proyecto 1: Intérprete MINILISP



Este repositorio contiene la implementación de un intérprete para MINILISP.

El intérprete está escrito en **Haskell** y utiliza las herramientas **Alex** para el análisis léxico y **Happy** para el análisis sintáctico.

##  Descripción del Pipeline

El sistema sigue un pipeline de compilación clásico para procesar el código MINILISP:

1.  **Entrada (String):** El usuario introduce un programa como una cadena de texto.
2.  **Lexer (`Lexer.x`):** La cadena se convierte en una secuencia de tokens (ej. `TokLP`, `TokPlus`, `TokInt 1`, `TokRP`).
3.  **Parser (`Parser.y`):** Los tokens se ensamblan en un **Árbol de Sintaxis Abstracta (ASA) de Superficie** (`Expr`). Esta es una representación directa del código escrito, con azúcar sintáctica (operadores variádicos, `let*`, `letrec`, etc.).
4.  **Desugarer (`Desugar.hs`):** El ASA de Superficie se traduce (desazucariza) a un **ASA de Núcleo** (`CoreExpr`). Este núcleo es mínimo y más simple, sin azúcar sintáctica (ej. `letrec` se convierte en `CLet` usando el combinador Y, los `lambda` variádicos se *currifican*, etc.).
5.  **Interpreter (`Interpreter.hs`):** El intérprete evalúa el ASA de Núcleo (`CoreExpr`) en un ambiente (`Env`) para producir un valor final (`Value`).

##  Estructura del Proyecto

* `Main.hs`: El punto de entrada principal. Contiene el **REPL** (menú interactivo) y la lógica para ejecutar los casos de prueba requeridos .
* `Lexer.x`: Definición del analizador léxico (tokens) usando **Alex** .
* `Parser.y`: Definición de la gramática y el ASA de Superficie (`Expr`) usando **Happy** .
* `Desugar.hs`: Lógica para la "desazucarización", traduciendo `Expr` a `CoreExpr`. Aquí es donde se maneja `let*`, `letrec`, `cond`, y operadores variádicos .
* `Interpreter.hs`: El evaluador principal que opera sobre el `CoreExpr` para producir valores .

##  Instalación y Compilación

Para compilar y ejecutar este proyecto, necesitarás **GHC (Haskell)**, **Alex** y **Happy**.

1.  **Instalar herramientas (si es necesario):**
    Asegúrate de tener `ghc`, `alex` y `happy` instalados y disponibles en tu PATH. Puedes usar GHCup o Cabal para instalarlos:
    ```bash
    # Ejemplo si usas cabal
    cabal install alex happy
    ```

2.  **Generar los archivos del Parser y Lexer:**
    Antes de compilar, debes ejecutar Alex y Happy para generar los archivos `.hs` a partir de los `.x` y `.y`.
    ```bash
    alex Lexer.x    # Genera Lexer.hs
    happy Parser.y  # Genera Parser.hs
    ```

3.  **Compilar el proyecto con GHC:**
    Usa `ghc --make` para compilar el `Main.hs` y todas sus dependencias, y genera un ejecutable llamado `minilisp`.
    ```bash
    ghc --make Main.hs -o minilisp
    ```

## ▶ Ejecución

Una vez compilado, puedes ejecutar el REPL interactivo simplemente corriendo el archivo ejecutable que generaste en el paso anterior:

```bash
./minilisp
```


## Autores

- Emiliano Figueroa Rojas 
- Carlos Manuel Gomez Calva
