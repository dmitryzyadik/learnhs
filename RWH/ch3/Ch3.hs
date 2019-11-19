
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

data Point2D = Point2D { x :: Double, y :: Double } deriving (Show)

instance Eq Point2D where
    (Point2D x1 y1) == (Point2D x2 y2) = y1 == y2 
    (Point2D x1 y1) /= (Point2D x2 y2) = y1 /= y2 
    
instance Ord Point2D where    
    (Point2D x1 y1) > (Point2D x2 y2) = (atan2 x1 y1) > (atan2 x2 y2)
    (Point2D x1 y1) < (Point2D x2 y2) = (atan2 x1 y1) < (atan2 x2 y2)
    (Point2D x1 y1) `compare` (Point2D x2 y2) = (atan2 x1 y1) `compare` (atan2 x2 y2)

data Direction = Left' | Right' | Straight' deriving (Show)

a = Point2D 1 2
b = Point2D 2 1
c = Point2D 3 2
d = Point2D 2 3
e = Point2D 3 4

pointList = [a,b,c,d,e]

lineAngle (Point2D x1 y1) (Point2D x2 y2) = atan2 (x2-x1)  (y2-y1)

{- filterPointByPolarAngle (Point2D x1 y1) (Point2D x2 y2) 
    | (atan2 x1 y1) > (atan2 x2 y2) = GT
    | otherwise                     = LG
 -}
subPoint2D :: Point2D -> Point2D -> Point2D 
subPoint2D  (Point2D x1 y1) (Point2D x2 y2) = (Point2D (x1 - x2) (y1 - y2))

addPoint2D :: Point2D -> Point2D -> Point2D 
addPoint2D  (Point2D x1 y1) (Point2D x2 y2) = (Point2D (x1 + x2) (y1 + y2))

dropDouble :: [Point2D] -> [Point2D]
dropDouble (p1:p2:px) 
    | p1 == p2 = p2 : dropDouble px

toBasePointList :: [Point2D] -> Point2D -> [Point2D]
toBasePointList [] p0       = []
toBasePointList [p1] p0     = [subPoint2D p1 p0]
toBasePointList [p1,p2] p0  = subPoint2D p1 p0 : subPoint2D p2 p0 : []
toBasePointList (p:ps) p0   = (subPoint2D p p0) : toBasePointList ps p0

fromBasePointList :: [Point2D] -> Point2D -> [Point2D]
fromBasePointList [] p0         = []
fromBasePointList [p1] p0       = [addPoint2D p1 p0]
fromBasePointList [p1,p2] p0    = addPoint2D p1 p0 : addPoint2D p2 p0 : []
fromBasePointList (p:ps) p0     = (addPoint2D p p0) : fromBasePointList ps p0
 

calcDirection :: Point2D -> Point2D -> Point2D -> Direction
calcDirection p1 p2 p3
    | la == ua = Straight'
    | la > ua  = Left'
    | otherwise = Right' 
        where   la = lineAngle p1 p2 
                ua = lineAngle p2 p3 
                

directionList :: [Point2D] -> [Direction]
directionList [] = []
directionList [a] = []
directionList [a,_] = []
directionList [a,b,c] = [(calcDirection a b c)]
directionList (a:b:c:d) = (calcDirection a b c) : (directionList (b:c:d))

findMin :: (Ord n) => [n] -> n
findMin = head  . sort 




pointsPairsSort pairs = sortBy comparison pairs
                            where comparison (angleA, distanceA, _) (angleB, distanceB, _) 
                                                | angleA > angleB = GT
                                                | angleA < angleB = LT
                                                | True = compare distanceA distanceB



