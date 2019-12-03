-- file: ch5/PrettyStub

import SimpleJSON

data Doc = ToBeDefined
        deriving (Show)

string :: String -> Doc        
string str = enclose '"' '"' . hcat . map oneChar

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined

fsep :: [Doc] -> Doc
fsep xs = undefined