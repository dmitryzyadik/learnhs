import Test.Hspec
import Test.QuickCheck

josephus :: [a] -> Int -> [a]
josephus [] _   = []
--josephus [x] _  = [x]
josephus xs k   = j : josephus js k
    where
        (j:js) = (focus' xs k)
     


focus' :: [a] -> Int  -> [a]
focus' xs 1 = xs
focus' (x:xs) k = focus' (xs ++ [x]) (k-1)  


--import Josephus (josephus)

main :: IO ()
main = hspec $ do
  describe "josephus" $ do
    it "works with integers" $ do
      josephus [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 1 `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
      josephus [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 2 `shouldBe` [2, 4, 6, 8, 10, 3, 7, 1, 9, 5]

    it "works with strings" $ do
      josephus "CodeWars" 4 `shouldBe` "esWoCdra"