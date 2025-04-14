import Data.Map qualified

data Exp = Val Int | Div Exp Exp | Var String | Let String Exp Exp

type Env = Data.Map.Map String Int

-- safediv :: Int -> Int -> Maybe Int
-- safediv _ 0 = Nothing
-- safediv x y = Just (div x y)

-- eval :: Exp -> Maybe Int
-- eval (Val n) = return n
-- eval (Div x y) = do
--   n <- eval x
--   m <- eval y
--   safediv n m

-- evalList' :: [Exp] -> [Maybe Int]
-- evalList' = map eval

-- evalList :: [Exp] -> Maybe [Int]
-- evalList [] = return []
-- evalList (e : exps) = do
--   res <- eval e
--   results <- evalList exps
--   return (res : results)

safediv :: Int -> Int -> Either String Int
safediv _ 0 = Left "Division by zero"
safediv x y = Right (div x y)

eval :: Exp -> Either String Int
eval (Val n) = return n
eval (Div x y) = do
  n <- eval x
  m <- eval y
  safediv n m

evalList' :: [Exp] -> [Either String Int]
evalList' = map eval

evalList :: [Exp] -> Either String [Int]
evalList [] = return []
evalList (e : exps) = do
  res <- eval e
  results <- evalList exps
  return (res : results)

eval' :: Env -> Exp -> Either String Int
eval' env (Val n) = return n

eval' env (Var s) = case Data.Map.lookup s env of
  Nothing -> error ("unknown: " ++ s)
  Just v -> return v

eval' env (Let s e1 e2) = do
  let v1 = eval' env e1
  case v1 of
    Left s -> error s
    Right v1 -> eval' (Data.Map.insert s v1 env) e2

eval' env (Div e1 e2) = do
  v1 <- eval' env e1
  v2 <- eval' env e2
  safediv v1 v2