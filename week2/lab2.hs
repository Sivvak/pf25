mapEither :: (a1 -> a2) -> (b1 -> b2) -> Either a1 b1 -> Either a2 b2
mapEither f g (Left a) = Left (f a)
mapEither f g (Right b) = Right (g b)

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : _) = Just x

-- generator
pita :: Integer -> [(Integer, Integer, Integer)]
pita n =
  [ (a, b, c)
    | a <- [1 .. n],
      b <- [a .. n],
      c <- [1 .. n],
      a ^ 2 + b ^ 2 == c ^ 2
  ]

-- trees
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

-- tree with 42 in each node
sameTree :: Int -> Tree Int
sameTree 0 = Empty
sameTree n = Node 42 (sameTree (n - 1)) (sameTree (n - 1))

-- fullTree 3 =
-- Node 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty))
--        (Node 6 (Node 5 Empty Empty) (Node 7 Empty Empty))

fullTree :: Int -> Tree Int
fullTree n = genTree n 0
  where
    genTree :: Int -> Int -> Tree Int
    genTree 0 _ = Empty
    -- genTree n i = let k = (i + 2 ^ (n - 1)) in Node k (genTree (n - 1) i) (genTree (n - 1) k)
    genTree n i = Node k (genTree (n - 1) i) (genTree (n - 1) k)
      where
        k = i + 2 ^ (n - 1)

toListNaive :: Tree a -> [a]
toListNaive Empty = []
toListNaive (Node a l r) = left ++ [a] ++ right
  where
    left = toListNaive l
    right = toListNaive r

-- to show that naive implementation of toList is very slow for left-skewed trees
leftTree :: Int -> Tree Int
leftTree 0 = Empty
leftTree n = Node 42 (leftTree (n - 1)) Empty

-- and much faster for right-skewed trees, it's because of lists concatenation
rightTree :: Int -> Tree Int
rightTree 0 = Empty
rightTree n = Node 42 Empty (rightTree (n - 1))

-- this implementation works in O(n) instead of O(n^2) because we just append new head to the result list
toList :: Tree a -> [a]
toList t = toListHelper t []
  where
    toListHelper :: Tree a -> [a] -> [a]
    toListHelper Empty xs = xs
    toListHelper (Node x l r) xs = toListHelper l (x : toListHelper r xs)

-- special case
-- toListHelper (Node x Empty Empty) xs = x:xs

-- combinators
data Expr
  = S
  | K
  | I
  | B
  | Expr :$ Expr
  | X
  | Z
  | V Int
  deriving (Show, Read)

-- it's a naive implementation, should be done better, similarly to the toList above
prettyExpr :: Expr -> Bool -> String
prettyExpr S _ = "S"
prettyExpr K _ = "K"
prettyExpr I _ = "I"
prettyExpr B _ = "B"
prettyExpr X _ = "X"
prettyExpr Z _ = "Z"
prettyExpr (V _) _ = "V_"
prettyExpr (e1 :$ e2) True = "(" ++ prettyExpr e1 False ++ " " ++ prettyExpr e2 True ++ ")"
prettyExpr (e1 :$ e2) False = prettyExpr e1 False ++ " " ++ prettyExpr e2 True