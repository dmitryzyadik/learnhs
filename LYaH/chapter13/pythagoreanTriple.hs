pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x = do 
    a <- [1..x]
    b <- [1..x]
    c <- [1..x]
    True <- return (a^2 + b^2 == c^2 && c > 0 && a < b)
    return (a,b,c)


