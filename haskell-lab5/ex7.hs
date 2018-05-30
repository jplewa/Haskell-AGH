newtype Box a = MkBox a deriving Show

instance Functor Box where
    fmap f (MkBox x) = MkBox (f x)

instance Applicative Box where
    pure = MkBox
    (MkBox f) <*> w = fmap f w

newtype MyTriple a = MyTriple (a,a,a) deriving Show

instance Functor MyTriple where
    fmap f (MyTriple (x,y,z)) = MyTriple (f x, f y, f z)

instance Applicative MyTriple where
    pure f = MyTriple (f,f,f)
    (MyTriple (f,g,h)) <*> w =  MyTriple (f w, g w, h w)