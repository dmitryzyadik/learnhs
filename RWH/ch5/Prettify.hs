module Prettify
    (
      Doc
    , empty
    , char
    , text
    , double
    , line
    , punctuate
    , hcat
    , fsep
    , (<>)
    , compact
    , pretty
    , fill
    ) where

import SimpleJSON
import Data.List (foldr1)
import Prelude hiding ((<>))

--import PutJSON

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (d:ds) =
              case d of
                Empty        -> best col ds
                Char c       -> c :  best (col + 1) ds
                Text s       -> s ++ best (col + length s) ds
                Line         -> '\n' : best 0 ds
                a `Concat` b -> best col (a:b:ds)
                a `Union` b  -> nicest col (best col (a:ds))
                                           (best col (b:ds))
          best _ _ = ""

          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                         where least = min width col

fill :: Int -> Doc -> Doc
fill width d = doc
  where (width', doc) = walk 0 d
        walk col d = -- walk :: Int -> Doc -> (Int
          case d of
            Empty         -> (col+0, Empty)
            Char c        -> (col+1, Char c)
            Text s        -> (col+length s, Text s)
            Line          -> (0, addSpaces (width-col))
            a `Union` b   -> (col, a `Union` b)
            a `Concat` b  -> (bWidth, a' `Concat` b')
              where (aWidth, a')    = walk col a
                    (bWidth, b')    = walk aWidth b
        addSpaces width 
                      = text (replicate width ' ') `Concat` Line 

nest :: Int -> Doc -> Doc
nest width x = snd (handleNode 0 x)
  where handleNode level d@(Char c) =
          case c of
            '[' -> (level + 1, d)
            '{' -> (level + 1, d)
            ']' -> (level - 1, d)
            '}' -> (level - 1, d)
            _   -> (level , d)
        handleNode level Line
          | level <= 0 = (level, Line)
          | otherwise = (level, Line `Concat` Text (replicate level ' '))
        handleNode level (a `Concat` b) =
          case handleNode level a of
            (level2, d2) -> case handleNode level2 b of
            (level3, d3) -> (level3, d2 `Concat` d3)
            
        handleNode level (a `Union` b) =
          (level, snd (handleNode level a) `Union` snd (handleNode level b))
        handleNode level d = (level, d)

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []          = []
punctuate p [d]         = [d]
punctuate p (d:ds)      = (d <> p) : punctuate p ds

data Doc = Empty
        | Char Char
        | Text String
        | Line
        | Concat Doc Doc
        | Union Doc Doc
            deriving (Show, Eq)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double d = text (show d)            

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
              case d of
                Empty        -> transform ds
                Char c       -> c : transform ds
                Text s       -> s ++ transform ds
                Line         -> '\n' : transform ds
                a `Concat` b -> transform (a:b:ds)
                _ `Union` b  -> transform (b:ds)

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs                