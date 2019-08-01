data Bit = Zero | One deriving (Eq)
data Sign = Minus | Plus deriving (Eq)
data Z = Z Sign [Bit]

type Flag = Bit

emptyZ  = Z Plus []
--add :: Z -> Z -> Z
--add (Z S L1) (Z S L2) = 
instance Eq Z where
    (Z s1 b1) == (Z s2 b2) = s1 == s2 && b1 == b2

add :: Z -> Z -> Z
add (Z signA a)  (Z signB b)  = Z signA (sumBits a b Zero)
--add _ _ = undefined
--add (Z Plus a)  (Z Minus b) = (Z Plus (sumBits a b Zero))
--add (Z Minus a) (Z Plus b)  = (Z Plus (sumBits a b Zero))
--add (Z Minus a) (Z Minus b) = (Z Plus (sumBits a b Zero))
    where 
        sumBits :: [Bit] -> [Bit] -> Bit -> [Bit]        
        sumBits []        []      flag    = []
        sumBits (a:as)    []      flag    = (fst $ add' newBit2 flag) : sumBits as [] newFlag2   
            where (newBit2, newFlag2) = (add' a flag )
        sumBits []        (b:bs)  flag    = (fst $ add' newBit3 flag) : sumBits [] bs newFlag3   
            where (newBit3, newFlag3) = (add' flag b ) 
        sumBits (a:as)    (b:bs)  flag    = (fst $ add' newBit flag)  : sumBits as bs newFlag  
            where (newBit, newFlag)   = (add' a b )

add _ _ = undefined                
                 
            -- | (Zero:as) (Zero:bs) Zero  = Zero : sum as bs Zero  
            -- | (One:as)  (One:bs) One     = Zero : sum as bs One  

mul :: Z -> Z -> Z
mul = undefined

add' :: Bit -> Bit -> (Bit, Flag)            
add' Zero Zero  = (Zero, Zero)
add' Zero One   = (One , Zero)
add' One  Zero  = (One , Zero)
add' One  One   = (Zero, One)

sub' :: Bit -> Bit -> (Bit, Flag)            
sub' Zero Zero   = (Zero, Zero)
sub' Zero One    = (One , Zero)
sub' One  Zero   = (One , Zero)
sub' One  One    = (One , One)

mul' :: Bit -> Bit -> (Bit, Flag)            
mul' Zero Zero   = (Zero, Zero)
mul' Zero One    = (Zero, Zero)
mul' One  Zero   = (Zero, Zero)
mul' One  One    = (One , Zero)

test001 = (add (Z Plus []) (Z Plus [])) == Z Plus []
test002 = (add (Z Plus []) (Z Plus [One])) == Z Plus [One]
test003 = (add (Z Plus []) (Z Minus [One])) == Z Minus [One]
{-}
test011 = (add (Z Plus [Zero, One, One]) (Z Plus [One])) == Z Plus [One, One, One]
test012 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One])) == Z Plus [Zero, Zero, Zero, One]
test013 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus [Zero, Zero, One, One]

test021 = (add (Z Minus [Zero, One, One]) (Z Minus [One])) == Z Minus [One, One, One]
test022 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One])) == Z Minus [Zero, Zero, Zero, One]
test023 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One, One]

test031 = (add (Z Minus [Zero, One, One]) (Z Plus [One])) == Z Minus [One, Zero, One]
test032 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One])) == Z Minus [Zero, Zero, One]
test033 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus []

test041 = (add (Z Plus [Zero, One, One]) (Z Minus [One])) == Z Plus [One, Zero, One]
test042 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One])) == Z Plus [Zero, Zero, One]
test043 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Plus []

test051 = (add (Z Plus [One]) (Z Minus [One])) == Z Plus []
test052 = (add (Z Plus [One]) (Z Minus [One, One])) == Z Minus [Zero, One]
test053 = (add (Z Plus [One]) (Z Minus [Zero, One])) == Z Minus [One]
test054 = (add (Z Plus [One]) (Z Minus [Zero, Zero, Zero, One])) == Z Minus [One, One, One]
test055 = (add (Z Plus [One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, Zero, One]
test056 = (add (Z Plus [Zero, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One]
test057 = (add (Z Plus [Zero, One]) (Z Minus [Zero, Zero, One])) == Z Minus [Zero, One]
test058 = (add (Z Plus [One, Zero, One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, One]


test101 = (mul (Z Plus []) (Z Plus [])) == emptyZ
test102 = (mul (Z Plus []) (Z Plus [One])) == emptyZ
test103 = (mul (Z Plus []) (Z Minus [One])) == emptyZ
test104 = (mul (Z Plus [One]) (Z Plus [])) == emptyZ
test105 = (mul (Z Minus [One]) (Z Plus [])) == emptyZ

test111 = (mul (Z Plus [One]) (Z Plus [One])) == Z Plus [One]
test112 = (mul (Z Minus [One]) (Z Plus [One])) == Z Minus [One]
test113 = (mul (Z Plus [One]) (Z Minus [One])) == Z Minus [One]
test114 = (mul (Z Minus [One]) (Z Minus [One])) == Z Plus [One]

test121 = (mul (Z Plus [One]) (Z Plus [Zero, One])) == Z Plus [Zero, One]
test122 = (mul (Z Plus [Zero, Zero, One]) (Z Plus [Zero, Zero, One])) == Z Plus [Zero, Zero, Zero, Zero, One]

test131 = (mul (Z Plus [One, Zero, One, Zero, One]) (Z Plus [One, One, One])) == Z Plus [One, One, Zero, Zero, One, Zero, Zero, One]


testAdd = test001 && test002 && test003 && test011 && test012 && test013 && test021 && test022 && test023 && test031 && test032 && test033 && test041 && test042 && test043 && test051 && test052 && test053 && test054 && test055 && test056 && test057 && test058
testMul = test101 && test102 && test103 && test104 && test105 && test111 && test112 && test113 && test114 && test121 && test122 && test131

testAll = testAdd && testMul
 -}--mul :: Z -> Z -> Z
--mul = undefined