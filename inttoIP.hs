import Data.Word (Word32)
import Data.List (intercalate)
import Data.Bits

type IPString = String

word32ToIP :: Word32 -> IPString
word32ToIP word32 = intercalate "." (bit32ToIP $ reverse $ take' 32 $ intToBit word32)

a = (.&.) 128 15


intToBit :: Word32 -> [Word32]
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


