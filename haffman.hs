import Data.List 
import Control.Arrow
import Data.Ord
 
data HTree a = Leaf a | Branch (HTree a) (HTree a)
    deriving (Show, Eq, Ord)
 
freq :: (Ord a) => [a] -> [(Int, a)]
freq = map(length &&& head). group. sort
 
serialize :: HTree d -> [(d, String)]
serialize (Branch l r) = map (second('0': )) (serialize l) ++ map (second('1': )) (serialize r)
serialize (Leaf x) = [(x, "")]
 
htree :: (Ord t, Num t) => [(t, HTree a)] -> HTree a
htree [(_, t)] = t
htree ((w1,t1) : (w2,t2):wts) =
    htree $ insertBy (comparing fst) (w1 + w2, Branch t1 t2) wts
 
huffman :: (Ord w, Num w) => [(w, a)] -> [(a, String)]
huffman = serialize. htree. sortBy (comparing fst). map (second Leaf)