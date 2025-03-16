data Tree a = Empty | Node a (Tree a) (Tree a)

instance (Show a) => Show (Tree a) where
  show :: (Show a) => Tree a -> String
  show Empty = "Empty"
  show (Node node l r) =
    showString "< "
      . shows node
      . showString " "
      . shows l
      . showString " | "
      . shows r
      . showString " >"
      $ ""

instance (Eq a) => Eq (Tree a) where
  (==) :: (Eq a) => Tree a -> Tree a -> Bool
  Empty == Empty = True
  (Node n1 l1 r1) == (Node n2 l2 r2) =
    n1 == n2 && l1 == l2 && r1 == r2
  _ == _ = False


data Exp
    = EInt Int             -- stała całkowita
    | EAdd Exp Exp         -- e1 + e2
    | EMul Exp Exp         -- e1 * e2
    | EVar String          -- zmienna
    | ELet String Exp Exp  -- let var = e1 in e2