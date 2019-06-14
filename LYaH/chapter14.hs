import Data.List (sort)
import Test.QuickCheck
import Test.Hspec

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char  
freeTree =   
    Node 'P'  
        (Node 'O'  
            (Node 'L'  
                (Node 'N' Empty Empty)  
                (Node 'T' Empty Empty)  
            )  
            (Node 'Y'  
                (Node 'S' Empty Empty)  
                (Node 'A' Empty Empty)  
            )  
        )  
        (Node 'L'  
            (Node 'W'  
                (Node 'C' Empty Empty)  
                (Node 'R' Empty Empty)  
            )  
            (Node 'A'  
                (Node 'A' Empty Empty)  
                (Node 'C' Empty Empty)  
            )  
        )  
data Direction = L | R deriving (Show)

type Breadcrumbs = [Direction]

goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goLeft (Node _ l _, bs) = (l, L:bs)

goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goRight (Node _ _ r, bs) = (r, R:bs)

x -: f = f x

data Shape = Square { side :: Double} 
            | Rectangle   { width :: Double, height :: Double }
            | Triangle    { base :: Double, height :: Double }
            | Circle      { radius :: Double }
            | CustomShape { area :: Double } 

instance Show Shape where
    show shape = show $ getArea shape
    
instance Eq Shape where
    s1 == s2 = (getArea s1) == (getArea s2)
    s1 /= s2 = not (s1 == s2)

instance Ord Shape where
    s1 `compare` s2 = (getArea s1) `compare` (getArea s2)
    s1 < s2 = (getArea s1) < (getArea s2)
    s1 <= s2 = (getArea s1) <= (getArea s2)
    s1 > s2 = (getArea s1) > (getArea s2)
    s1 >= s2 = (getArea s1) >= (getArea s2) 

getArea :: Shape -> Double
getArea (Square side)       = side ^ 2
getArea (Rectangle w h)     = w * h
getArea (Triangle b h)      = b * (h / 2)
getArea (Circle radius)     = pi * radius^2
getArea (CustomShape  area) = area

shapes :: [Shape]
shapes = [
    CustomShape area,
    Square side,
    Circle radius,
    Triangle triangleBase1 triangleHeight1,
    Triangle triangleBase2 triangleHeight2,
    Rectangle width height,
    CustomShape area2]
    where
      area = 1.1234
      side = 1.1234
      radius = 1.1234
      triangleBase1 = 2.0
      triangleHeight1 = 5.0
      triangleBase2 = 3.0
      triangleHeight2 = 4.0
      width = 4
      height = 4
      area2 = 16.1

shapes' :: [Shape]
shapes' = [ CustomShape 15.673063743755279                --  15.673063743755279
        , Rectangle 2.01488912603621 24.197796350094986 --  48.75587673984508
        , Triangle 29.268710378065993 4.936875202650166 --  72.24798523951178
        , Triangle 64.46167106615941 4.092651352236228  -- 131.90957262816212
        , Rectangle 23.4477504800821 7.701123869846373  -- 180.574030916362
        , Triangle 12.46499126092774 52.65998521541476] -- 328.20312775536445

main :: IO ()
main = hspec $
  describe "Example Tests" $
    it "should work with example tests" $
        forAll (shuffle shapes) (\ss -> sort shapes == shapes)