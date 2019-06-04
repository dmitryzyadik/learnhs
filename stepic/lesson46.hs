import Control.Monad.Writer
--import Data.Monoid

type Endo a = a -> a

{-newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Monoid Xor where
    mempty              = Xor False
    mappend (Xor True) (Xor True)   = Xor False
    mappend (Xor False) (Xor False) = Xor False  
    mappend (Xor _) (Xor _)         = Xor True  
    -}
{-newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Monoid a => Monoid (Maybe' a) where
    mempty = mempty
    (Maybe' a) `mappend` (Maybe' b) = Maybe'( a `mappend` b)-}

data Point3D a = Point3D a a a deriving Show
instance Functor Point3D where
    fmap f (Point3D a b c) = Point3D (f a) (f b) (f c) 

data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a) deriving Show
instance Functor GeomPrimitive where
    fmap f (Point p) = Point (fmap f p) 
    fmap f (LineSegment p p2) = LineSegment ((fmap f p)) ((fmap f p2))

data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show    
instance Functor Tree where
    fmap f (Leaf Nothing)           = (Leaf Nothing)
    fmap f (Leaf (Just a))          = (Leaf (Just (f a)))
    fmap f (Branch l (Nothing) r)   = Branch (fmap f l) (Nothing) (fmap f r)
    fmap f (Branch l (Just a) r)    = Branch (fmap f l) (Just (f a)) (fmap f r)
    --fmap f (Point (Point3D a b c)) = Point (Point3D (f a) (f b) (f c)) 
    --fmap f (LineSegment (Point3D a b c) (Point3D a1 b1 c1)) = LineSegment (Point3D (f a) (f b) (f c) ) (Point3D (f a1) (f b1) (f c1) )