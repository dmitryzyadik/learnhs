import Data.Char
import Data.List

revRange :: (Char,Char) -> [Char]
revRange (a,b) = reverse $ unfoldr (\x -> if x > b then Nothing else Just (x, chr (ord x + 1))) a
  