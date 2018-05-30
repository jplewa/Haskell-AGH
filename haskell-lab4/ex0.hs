type FirstName = String

type LastName = String

data Person = MkPerson FirstName LastName deriving (Show)

data Person2  = Person2
    {
        n :: String
    ,   s :: String
    } deriving (Show)

data MkDouble = MkMyDouble Double

instance Eq MkDouble where
    (==) (MkMyDouble d1) (MkMyDouble d2) = d1 == d2

instance Ord MkDouble where
    (<=) (MkMyDouble d1) (MkMyDouble d2) = d1 <= d2

data BinTree a =    Leaf a 
                    | Node (BinTree a) (BinTree a)
                    deriving (Show)

sumTree :: (Num a) => BinTree a -> a
sumTree (Leaf n) = n
sumTree (Node t1 t2) = sumTree t1 + sumTree t2