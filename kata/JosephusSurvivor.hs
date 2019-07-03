import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Text.Printf (printf)

josephusSurvivor :: Int -> Int -> Int
josephusSurvivor n k = josephus [1..n] k


josephus :: [Int] -> Int -> Int
josephus [] _   = 0
josephus [x] _  = x
josephus xs k   = josephus js k 
    where
        js = (focus' xs k k)   

focus' :: [a] -> Int -> Int -> [a]
focus' [x] _ _ = [x]
focus' (x:xs) 1 n = focus' xs n n
focus' (x:xs) k n = focus' (xs ++ [x]) (k-1) n        

focus'' :: [a] -> Int -> Int -> [a]
focus'' [x] _ _ = [x]
focus'' (x:xs) 1 n = focus'' xs n n
focus'' (x:xs) k n = focus'' (xs ++ [x]) (k-1) n        

type ListZipper a = ([a],[a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs , x:bs)

loop' :: (ListZipper a -> ListZipper a) -> ListZipper a -> Int -> ListZipper a
loop' f n 1 = n
loop' f n c = loop' f (f n) (c-1)

loop''  :: (ListZipper a -> ListZipper a) -> Int -> ListZipper a -> ListZipper a
loop'' f 1 n = n
loop'' f c n = loop'' f (pred c) (f $! n) 


josephusSurvivorSol :: Int -> Int -> (Int, String)
josephusSurvivorSol n k = foldl' (\(x,s) v -> (mod (k+x) v, s ++ " " ++ show (k+x) ++ " mod " ++ show v ++ " = " ++ show (mod (k+x) v) ++ " " )) (0, []) [2..n]

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f x0 []      =  x0
foldl' f x0 (x:xs)  =  foldl f (f x0 x) xs

testJos ::  Int -> Int -> Int -> Spec
testJos n k s = 
    it (printf "should return josephusSurvivor for n k result : %d %d --> %d \n" n k  s) $
        josephusSurvivor n k `shouldBe` s

main = hspec $ modifyMaxSuccess (const 100) $ do
    
    describe "closest" $ do
        testJos 7 3 4
        testJos 11 19 10
        testJos 40 3 28
        testJos 14 2 13
        testJos 100 1 100
        testJos 300 300 265