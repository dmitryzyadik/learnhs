module Quadratic where
        
    import Data.Complex
    
    type RootT = Complex Double
    
    data Roots = Roots RootT RootT deriving Show

    data Quadratic = Quadratic { a :: Double, b :: Double, c :: Double } deriving Show

    roots :: Quadratic -> Roots
    roots (Quadratic 0 0 _)     = error "Not a quadratic polynaminal"
    roots (Quadratic 0.0 b c)   = let root = ((-c) / b :+ 0) in Roots root root
    roots (Quadratic a b c)     = let discriminant = (b * b - 4 * a * c) in rootsInternal (Quadratic a b c) discriminant 

    rootsInternal :: Quadratic -> Double -> Roots
    rootsInternal q d | d == 0 = let r = (-(b q) / 2.0 / (a q))
                                     root = r :+ 0
                                 in Roots root root

    rootsInternal q d | d < 0 = Roots (realpart :+ complexpart) (realpart :+ (-complexpart))
        where   plusd = -d
                twoa = 2.0 * (a q)
                complexpart = (sqrt plusd) / twoa
                realpart = - (b q) / twoa
    rootsInternal q d = Roots (root1 :+ 0 ) (root2 :+ 0)
        where   plusd = -d
                twoa = 2.0 * (a q)
                dpart = (sqrt plusd) / twoa
                prefix = - (b q) / twoa
                root1 = prefix + dpart
                root2 = prefix - dpart