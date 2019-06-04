data Bit = Zero | One
data Sign = Minus | Plus
data Z = Z Sign [Bit]

add :: Z -> Z -> Z
add a b = sum a b Zero
    where 
        sum :: Z -> Z -> Bit -> Z
        sum a b s 
            | (One:as) (One:bs) One     = Zero : sum as bs One  
            | (Zero:as) (Zero:bs) Zero  = Zero : sum as bs Zero  
            | (One:as) (One:bs) One     = Zero : sum as bs One  

add' :: Bit -> Bit -> (Bit,Bit)            
add' Zero Zero   = (Zero, Zero)
add' Zero One    = (One , Zero)
add' One  Zero   = (One , Zero)
add' One  One    = (Zero, One)

sub' :: Bit -> Bit -> (Bit,Bit)            
sub' Zero Zero   = (Zero, Zero)
sub' Zero One    = (One , Zero)
sub' One  Zero   = (One , Zero)
sub' One  One    = (One , One)

mul' :: Bit -> Bit -> (Bit,Bit)            
mul' Zero Zero   = (Zero, Zero)
mul' Zero One    = (Zero, Zero)
mul' One  Zero   = (Zero, Zero)
mul' One  One    = (One , Zero)


--mul :: Z -> Z -> Z
--mul = undefined