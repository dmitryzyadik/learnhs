import Data.Char (toUpper)

letters :: [(Char, String)]
letters =  [
    ('A', "Alpha"),  ('B', "Bravo"),   ('C', "Charlie"),
    ('D', "Delta"),  ('E', "Echo"),    ('F', "Foxtrot"),
    ('G', "Golf"),   ('H', "Hotel"),   ('I', "India"),
    ('J', "Juliett"),('K', "Kilo"),    ('L', "Lima"),
    ('M', "Mike"),   ('N', "November"),('O', "Oscar"),
    ('P', "Papa"),   ('Q', "Quebec"),  ('R', "Romeo"),
    ('S', "Sierra"), ('T', "Tango"),   ('U', "Uniform"),
    ('V', "Victor"), ('W', "Whiskey"), ('X', "X-ray"),
    ('Y', "Yankee"), ('Z', "Zulu")
  ]

nato :: String -> String
nato []         = []
nato str@(h:[]) = (find letters (toUpper h))
nato str@(h:t)  = (find letters (toUpper h)) ++ " " ++ (nato t)

find :: [(Char, String)] -> Char -> String
find [] _ = []
find ((c,s):xs) f = if c == f then s else find xs f