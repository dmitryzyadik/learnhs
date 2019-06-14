import Data.Word (Word32)
import Data.List (intercalate)
import Data.Bits
import Control.Monad.Writer

type IPString = String



--word32ToIP :: Word32 -> IPString
--word32ToIP word32 = intercalate "." (bit32ToIP $ reverse $ take' 32 $ intToBit word32)
word32ToIP :: Word32 -> IPString
word32ToIP word32 = (show (a x4) ++ "." ++ (show (a x3)) ++ "." ++ (show (a x2)) ++ "." ++ (show (a x1)))  
    where 
        [x4, x3, x2, x1] = [s x3, s x2, s word32, word32]

        
help' :: (Word32 -> Word32) -> Word32 -> Writer String Word32        
help' f a = do
    let res = f a
    tell "info"
    return res

    

a :: Word32 -> Word32
a x = (.&.) x 0xff

s ::  Word32 -> Word32
s x = shiftR x 8


{- intToBit :: Word32 -> [Word32]
intToBit 0 = []
intToBit 1 = [1]
intToBit x = (x `mod` 2) : intToBit (x `div` 2)

take' :: Word32 -> [Word32] -> [Word32]
take' 0 x = []
take' n []  = 0 : take' (n-1) []
take' n (x:xs)  = x : take' (n-1) xs

bitToInt [] = 0
bitToInt x@(h:t) = (h * (2 ^ ((length x)-1))) + (bitToInt t)

--bit32ToIP $ reverse $ take' 32 $ intToBit
bit32ToIP :: [Word32] -> [String]
bit32ToIP [] = []
bit32ToIP x = (show $ bitToInt $ take 8 x) : bit32ToIP (drop 8 x)
 -}

