humanReadable :: Int -> String
humanReadable x 
  | x >= 0 = show' hh ++ ":" ++ show' mm ++ ":" ++ show' ss
  where 
    show' :: Int -> String
    show' s 
        | s < 10 = "0" ++ show s
        | otherwise = show s
    hh = x `div` 3600
    mm = x `mod` 3600 `div` 60    
    ss = x `mod` 3600 `mod` 60