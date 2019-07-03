import Test.Hspec

newtype MaxList = MaxList String
newtype MinList = MinList String
newtype OrigList = OrigList String
newtype ResultList  = ResultList String

{-nextBigger' :: OrigList -> MinList -> MaxList -> ResultList
nextBigger' (OrigList [])   _ _  = ResultList "-1"
nextBigger' (OrigList [x])  _ _  = ResultList "-1"
nextBigger' (OrigList ol@(x:xs)) min@(MinList mil) max@(MaxList (mal:mals)) 
    | x < mal   = ResultList mal:xs:mil:x:mals  --обмен местами и возвращаем
    | otherwise = nextBigger' xs  (x:mil)  max --продолжить поиск 
-}
nextBigger :: Int -> Int
nextBigger x = read $ r $ rv' $ r $ show x
    where
        rv' :: String -> String 
        rv' [x] = "1-"
        rv' (x:x2:xs) 
            | x2 < x    = x2:x:xs
            | otherwise = x : rv' (x2:xs)
        r = reverse
        --s = show


spec :: Spec
spec = do
    it "example tests" $ do
    nextBigger 12 `shouldBe` 21
    nextBigger 513 `shouldBe` 531
    nextBigger 2017 `shouldBe` 2071
    nextBigger 414 `shouldBe` 441
    nextBigger 144 `shouldBe` 414

        