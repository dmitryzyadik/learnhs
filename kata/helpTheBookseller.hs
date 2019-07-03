data Stock = Stock [Char] Int 

stocklist :: [Stock] -> [Char] -> [(Char, Int)]
stocklist [] _ = []
stocklist st cs = map (\x -> help' st (x,0)) cs 

help' :: [Stock] -> (Char, Int) -> (Char, Int)    
help' [] ls = ls
help' ((Stock label bookInStore):st) (letter, sum)  
    = if letter == (head label) then help' st (letter, bookInStore + sum) else help' st (letter, sum)



retTest = [('A',200), ('B',1140)]

--test :: [Char]
test = ['A','B'] 

stock = [Stock "ABAR" 200, Stock "CDXE" 500, Stock "BKWR" 250, Stock "BTSQ" 890, Stock "DRTY" 600]     