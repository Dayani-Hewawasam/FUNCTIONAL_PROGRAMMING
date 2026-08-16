module Expr where

-- ============================================================
-- PART A — Language Design
-- ============================================================

-- Expr is a sum type: numeric literals, variable references,
-- the four arithmetic operators, PLUS one extension constructor
-- (Let) that gives Expr a genuine second "shape" (a binding form,
-- not just another arithmetic op). This satisfies the "real sum
-- type, not a single-constructor record" requirement.
data Expr
  = Lit Double                  -- a number, e.g. 3.0
  | Var String                  -- a variable reference, e.g. "x"
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Let String Expr Expr        -- Let x = e1 in e2   (extension constructor)
  deriving (Show, Eq)

-- Variable bindings: a simple association list of (name, value)
type Env = [(String, Double)]


-- ============================================================
-- PART B — Evaluation
-- ============================================================

-- eval recurses over EVERY constructor of Expr (one equation each,
-- exactly the "one equation per constructor" recipe from lectures).
-- Errors are reported as (Left errorMessage) instead of crashing —
-- this is the Maybe/Either "failure as a value" pattern from the
-- ADTs & Abstraction session, just with an error message attached.
eval :: Env -> Expr -> Either String Double
eval _   (Lit n) = Right n

eval env (Var x) =
  case lookup x env of
    Just v  -> Right v
    Nothing -> Left ("undefined variable: " ++ x)

eval env (Add l r) = binOp (+) env l r
eval env (Sub l r) = binOp (-) env l r
eval env (Mul l r) = binOp (*) env l r

eval env (Div l r) = do
  lv <- eval env l
  rv <- eval env r
  if rv == 0
    then Left "division by zero"
    else Right (lv / rv)

eval env (Let x e1 e2) = do
  v <- eval env e1                 -- evaluate the bound expression first
  eval ((x, v) : env) e2           -- then evaluate the body with x bound

-- Small helper shared by Add/Sub/Mul: evaluate both sides, then combine.
-- This uses the Either monad's do-notation so errors short-circuit
-- automatically (if `eval env l` fails, `eval env r` never even runs).
binOp :: (Double -> Double -> Double) -> Env -> Expr -> Expr -> Either String Double
binOp op env l r = do
  lv <- eval env l
  rv <- eval env r
  Right (op lv rv)


-- ============================================================
-- PART C — Higher-Order Functions
-- ============================================================

-- simplify rewrites at least three algebraic identities.
-- It recurses INTO subexpressions first (so nested redundancies
-- get cleaned up too), then checks the identities at the top level.
simplify :: Expr -> Expr
simplify (Add l r) = simplifyAdd (simplify l) (simplify r)
simplify (Sub l r) = simplifySub (simplify l) (simplify r)
simplify (Mul l r) = simplifyMul (simplify l) (simplify r)
simplify (Div l r) = Div (simplify l) (simplify r)
simplify (Let x e1 e2) = Let x (simplify e1) (simplify e2)
simplify e = e   -- Lit and Var are already as simple as possible

simplifyAdd :: Expr -> Expr -> Expr
simplifyAdd l (Lit 0) = l              -- x + 0 -> x
simplifyAdd (Lit 0) r = r              -- 0 + x -> x
simplifyAdd l r       = Add l r

simplifySub :: Expr -> Expr -> Expr
simplifySub l (Lit 0) = l              -- x - 0 -> x
simplifySub l r
  | l == r    = Lit 0                  -- x - x -> 0
  | otherwise = Sub l r

simplifyMul :: Expr -> Expr -> Expr
simplifyMul _ (Lit 0) = Lit 0          -- x * 0 -> 0
simplifyMul (Lit 0) _ = Lit 0          -- 0 * x -> 0
simplifyMul l (Lit 1) = l              -- x * 1 -> x
simplifyMul (Lit 1) r = r              -- 1 * x -> x
simplifyMul l r       = Mul l r


-- Evaluate a batch of expressions against a shared environment,
-- keeping only the ones that succeeded (Left errors are dropped).
-- Demonstrates: currying/partial application (`eval env` is a
-- partially-applied function passed straight to map), plus a
-- map + filter/foldr pipeline exactly like the lecture pipelines.
evalBatch :: Env -> [Expr] -> [Double]
evalBatch env exprs =
  [ v | Right v <- map (eval env) exprs ]
  -- map (eval env) exprs  :: [Either String Double]   <-- currying: `eval env`
  -- the list comprehension's pattern `Right v <-` keeps only successes,
  -- exactly like `filter` would, but pattern-matching directly on Either.

-- A second small HOF-based function required by the brief: report
-- how many expressions succeeded vs failed, using foldr.
batchReport :: Env -> [Expr] -> (Int, Int)
batchReport env exprs = foldr count (0, 0) (map (eval env) exprs)
  where
    count (Right _) (s, f) = (s + 1, f)
    count (Left _)  (s, f) = (s, f + 1)


-- ============================================================
-- Sample data & a small runnable demo (for "how to run" + the
-- 5 sample evaluations required in the report)
-- ============================================================

sampleEnv :: Env
sampleEnv = [("x", 10), ("y", 2)]

-- (x + 3) * y
sample1 :: Expr
sample1 = Mul (Add (Var "x") (Lit 3)) (Var "y")

-- x / 0   -> should fail with division by zero
sample2 :: Expr
sample2 = Div (Var "x") (Lit 0)

-- z + 1   -> should fail with undefined variable
sample3 :: Expr
sample3 = Add (Var "z") (Lit 1)

-- let a = 5 in a * a
sample4 :: Expr
sample4 = Let "a" (Lit 5) (Mul (Var "a") (Var "a"))

-- x + 0   (should simplify to just x)
sample5 :: Expr
sample5 = Add (Var "x") (Lit 0)

main :: IO ()
main = do
  putStrLn "== eval demos =="
  print (eval sampleEnv sample1)   -- Right 26.0
  print (eval sampleEnv sample2)   -- Left "division by zero"
  print (eval sampleEnv sample3)   -- Left "undefined variable: z"
  print (eval sampleEnv sample4)   -- Right 25.0

  putStrLn "\n== simplify demo =="
  print sample5                    -- Add (Var "x") (Lit 0)
  print (simplify sample5)         -- Var "x"

  putStrLn "\n== evalBatch / batchReport demo =="
  let batch = [sample1, sample2, sample3, sample4]
  print (evalBatch sampleEnv batch)     -- [26.0,25.0]  (failures dropped)
  print (batchReport sampleEnv batch)   -- (2,2)
