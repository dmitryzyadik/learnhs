
module Ch3 where

import Data.List




{- Exercise
1. Write the converse of fromList for the List type: a function that takes a 
List a and generates a [a]
-}

{- 
2. Define a tree type that has only one constructor, like our Java example. 
Instead of the Empty constructor, use the Maybe type to refer to a 
node's children -}

--data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))  deriving (Show)

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)
            
{- simpleTree = Node "parent"  (Just (Node "left child" Nothing Nothing))
                            ( Nothing)
 -}
data List a = Cons a (List a)
            | Nil
                deriving (Show)

list = Cons 3 (Cons 2 (Cons 1 (Cons 0 Nil))) 

fromList :: List a -> [a]
fromList Nil        = []
fromList (Cons a xs)  = a : fromList xs 

{- Exercises
1. Write a function that computes the number of elements in a list. 
To test it, ensure that it gives the same answers as the 
standard length function. -}

--length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs


sumList :: (Num a) => [a] -> a
sumList [] = 0
sumList (x:xs) = x + sumList xs

meanList [] = 0
meanList l = (sumList l) `div` (length l)

palindrome [] = []
palindrome a = a ++ (reverse' a)

reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x] 

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = False
isPalindrome [x] = False
isPalindrome x | (reverse' x) == x = True
isPalindrome _ = False 

intersperse' :: a -> [[a]] -> [a]
intersperse' s [] = []
intersperse' s (x:[]) = x
intersperse' s (x:xs) = x ++ [s] ++ (intersperse' s xs) 

t  = Node "x" Empty Empty
t1 = Node "x" Empty (Node "y" Empty Empty)

heightTree :: Tree a -> Int
heightTree Empty = 0
heightTree (Node _ l r) = 1 + max (heightTree l) (heightTree r) 


data Point = Point Float Float

data Direction = Left' | Right' | Straight'

angleSide :: Point -> Point -> Point -> Direction
angleSide (Point ax ay) (Point bx by) (Point cx cy) | (ax > cx) && (cx > bx) = Right'
angleSide (Point ax ay) (Point bx by) (Point cx cy) | (ax < cx) && (cx < bx) = Right'
angleSide _ _ _ = Left'
