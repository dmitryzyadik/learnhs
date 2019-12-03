--file ch04/Fold.hs

hiddenInsine x y = someFunc (x `seq` y)

hiddenByLet x y z = let a = x `seq` someFunc y
                    in anotherFinc a z

inTheOutside x y = x `seq`  someFunc y                    
