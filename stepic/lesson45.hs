data List a = Nil | Cons a (List a) deriving (Show)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons a b) = [a] ++ (fromList b)

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)

data Nat = Zero | Suc Nat

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add Zero b = b
add (Suc a) b = Suc $ add a b

mul :: Nat -> Nat -> Nat
mul Zero b = Zero
mul (Suc Zero) b = b
mul (Suc a) b = add (mul a b) b

fac :: Nat -> Nat
fac Zero = (Suc Zero)
fac (Suc Zero) = (Suc Zero)
fac (Suc a)    = mul (Suc a) (fac a)  

data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf a)     = 0
height (Node a b)   = 1 + (max (height a) (height b)) 

size :: Tree a -> Int
size (Leaf a)     = 1
size (Node a b)   =(size a) + (size b) + 1 

--data Tree a = Leaf a | Node (Tree a) (Tree a)
size' :: Tree a -> Int
size' (Leaf a)     = 1
size' (Node a b)   =(size' a) + (size' b)

size'' :: Tree Int -> Int
size'' (Leaf a)     = a
size'' (Node a b)   =(size'' a) + (size'' b)


avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go a = (,) (size' a) (size'' a)

infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand ((e1 :+: e2) :*: e) = expand e1 :*: expand e :+: expand e2 :*: expand e
expand (e :*: (e1 :+: e2)) = expand e :*: expand e1 :+: expand e :*: expand e2
expand (e1 :+: e2) = expand e1 :+: expand e2
expand (e1 :*: e2) = expand e1 :*: expand e2
expand e = e