class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a    
    stompOrStab a 
        | (doesEnrageMork a && doesEnrageMork a)    == True = stab $ stomp a    
        | doesEnrageMork a                          == True = stomp a
        | doesEnrageMork a                          == True = stab a        
        | otherwise                                         = a