module Desugar
  ( Core(..)
  , Op1(..), Op2(..)
  , desugar
  ) where

import Parser (Expr(..))

-- Núcleo mínimo
data Op1 = opAdd1 | opSub1 | opSqrt | opNot | opHead | opTail
         deriving (Eq, Show)

data Op2 = opAdd | opSub | opMul | opDiv
         | opEq | opNe | opLt | opGt | opLe | opGe
         | opExpt
         deriving (Eq, Show)

data Core
  = CVar String
  | CInt Integer
  | CBool Bool
  | CLam String Core                 -- lambda 1 parámetro (curried)
  | CApp Core Core                   -- aplicación binaria (izq-asoc)
  | CIf Core Core Core
  | CPair Core Core
  | CFst Core
  | CSnd Core
  | CNil
  | CCons Core Core
  | CPrim1 Op1 Core
  | CPrim2 Op2 Core Core
  | CLet String Core Core            -- let secuencial (1 binding)
  | CLetRec String Core Core         -- letrec f = e1 in e2
  deriving (Eq, Show)

-- Helpers
mkLams :: [String] -> Core -> Core
mkLams []     b = b
mkLams (x:xs) b = CLam x (mkLams xs b)

mkApps :: Core -> [Core] -> Core
mkApps = foldl CApp

mkLetsSeq :: [(String, Core)] -> Core -> Core
mkLetsSeq [] body = body
mkLetsSeq ((x,e):xs) body = CLet x e (mkLetsSeq xs body)

-- Para let paralelo: evaluamos RHS a temporales y luego asignamos
mkLetsPar :: [(String, Core)] -> Core -> Core
mkLetsPar binds body =
  let temps    = [("_p" ++ show i) | i <- [0..length binds - 1]]
      rhss     = map snd binds
      names    = map fst binds
      -- primero: let _p0 = e0 in let _p1 = e1 in ...
      staged   = mkLetsSeq (zip temps rhss)
                -- luego: let x0 = _p0 in let x1 = _p1 in ...
                . mkLetsSeq (zip names (map CVar temps))
  in staged body

foldNary2 :: Op2 -> [Core] -> Core
foldNary2 _  []       = error "operación binaria sin argumentos"
foldNary2 _  [_]      = error "operación binaria requiere >=2"
foldNary2 op (a:b:xs) = foldl (CPrim2 op) (CPrim2 op a b) xs

-- Desugar principal
desugar :: Expr -> Core
-- átomos
desugar (Var x)         = CVar x
desugar (IntLit n)      = CInt n
desugar (BoolLit b)     = CBool b
-- pares y selectores
desugar (Pair a b)      = CPair (desugar a) (desugar b)
desugar (Fst e)         = CFst (desugar e)
desugar (Snd e)         = CSnd (desugar e)
-- listas (azúcar)
desugar (List xs)       = foldr (CCons . desugar) CNil xs
desugar (Head e)        = CPrim1 opHead (desugar e)
desugar (Tail e)        = CPrim1 opTail (desugar e)
-- if / if0
desugar (If c t e)      = CIf (desugar c) (desugar t) (desugar e)
desugar (If0 c t e)     = CIf (CPrim2 opEq (desugar c) (CInt 0))
                              (desugar t) (desugar e)
-- lambda multi-params -> curry
desugar (Lambda ps b)   = case ps of
  []   -> error "lambda sin parámetros"
  args -> mkLams args (desugar b)
-- aplicación con N args -> left fold
desugar (App f args)    = mkApps (desugar f) (map desugar args)
-- let paralelo / let* secuencial / letrec
desugar (Let bs body)       = mkLetsPar  (map (\(x,e) -> (x, desugar e)) bs) (desugar body)
desugar (LetStar bs body)   = mkLetsSeq  (map (\(x,e) -> (x, desugar e)) bs) (desugar body)
desugar (LetRec f e1 e2)    = CLetRec f (desugar e1) (desugar e2)
-- cond: cascada de if; último es else (tu gramática asegura ElseClause)
desugar (Cond clausesElse)  =
  let go []                     = error "cond sin cláusulas"
      go [(g,b)]               = CIf (desugar g) (desugar b) (error "cond sin else")
      -- Por tu gramática, el último par suele ser (True, elseExpr)
      go ((g,b):rest@((BoolLit True, eElse):_)) =
          CIf (desugar g) (desugar b) (desugar eElse)
      go ((g,b):rest) =
          CIf (desugar g) (desugar b) (go rest)
  in go clausesElse
-- unarios
desugar (Add1 e)        = CPrim1 opAdd1 (desugar e)
desugar (Sub1 e)        = CPrim1 opSub1 (desugar e)
desugar (Sqrt e)        = CPrim1 opSqrt (desugar e)
desugar (Not e)         = CPrim1 opNot (desugar e)
-- binarios específicos
desugar (Expt a b)      = CPrim2 opExpt (desugar a) (desugar b)
-- n-arios (se pliegan a binarios)
desugar (Add xs)        = foldNary2 opAdd (map desugar xs)
desugar (Sub xs)        = foldNary2 opSub (map desugar xs)
desugar (Mul xs)        = foldNary2 opMul (map desugar xs)
desugar (Div xs)        = foldNary2 opDiv (map desugar xs)
desugar (Eq  xs)        = foldNary2 opEq  (map desugar xs)
desugar (Ne  xs)        = foldNary2 opNe  (map desugar xs)
desugar (Lt  xs)        = foldNary2 opLt  (map desugar xs)
desugar (Gt  xs)        = foldNary2 opGt  (map desugar xs)
desugar (Le  xs)        = foldNary2 opLe  (map desugar xs)
desugar (Ge  xs)        = foldNary2 opGe  (map desugar xs)
