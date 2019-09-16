charConcat :: String -> String
charConcat s = zipper s (reverse s) 1 ((length s) `div` 2 )

zipper :: String -> String -> Int -> Int -> String
zipper (x:[]) (y:ys) _ _ = []
zipper (x:xs) (y:[]) _ _ = []
zipper (x:xs) (y:ys) n l = (x : y : (show n) ++ (if n < l then (zipper xs ys (n+1) l) else []))
