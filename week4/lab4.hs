data Nat = Zero | S Nat deriving (Show)

-- test expressions
one = S Zero

two = S one

three = S two

instance Enum Nat where
  toEnum :: Int -> Nat
  toEnum 0 = Zero
  toEnum n = S (toEnum (n - 1))

  fromEnum :: Nat -> Int
  fromEnum Zero = 0
  fromEnum (S n) = 1 + fromEnum n

add :: Nat -> Nat -> Nat
add m Zero = m
add m (S n) = S (add m n)

mul :: Nat -> Nat -> Nat
mul m Zero = Zero
mul m (S n) = add (mul m n) m

pow :: Nat -> Nat -> Nat
pow m Zero = S Zero
pow m (S n) = mul m (pow m n)

eqNat :: Nat -> (Nat -> Bool)
eqNat Zero Zero = True
eqNat (S n1) (S n2) = eqNat n1 n2
eqNat _ _ = False

foldn :: (a -> a) -> a -> Nat -> a
foldn h c Zero = c
foldn h c (S n) = h (foldn h c n)

fact :: Nat -> Nat
fact = snd . foldn f (Zero, S Zero)
  where
    f :: (Nat, Nat) -> (Nat, Nat)
    f (m, n) = (S m, mul (S m) n)

fib :: Nat -> Nat
fib = snd . foldn g (Zero, S Zero)
  where
    g :: (Nat, Nat) -> (Nat, Nat)
    g (m, n) = (n, add m n)

npred :: Nat -> Nat
npred Zero = error "Predecessor of zero isn't natural"
npred (S n) = foldn S Zero n