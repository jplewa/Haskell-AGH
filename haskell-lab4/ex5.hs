newtype MyInt = MkMyInt Int

instance Eq MyInt where
    (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2

instance Ord MyInt where
    (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2

instance Num MyInt where
    (+) (MkMyInt i1) (MkMyInt i2)   = MkMyInt (i1 + i2)
    (-) (MkMyInt i1) (MkMyInt i2)   = MkMyInt (i1 - i2)
    (*) (MkMyInt i1) (MkMyInt i2)   = MkMyInt (i1 * i2)
    negate (MkMyInt i)              = MkMyInt (negate i)
    abs (MkMyInt i)                 = MkMyInt (abs i)
    signum (MkMyInt i)              = MkMyInt (signum i)
    fromInteger int                 = MkMyInt (fromIntegral int)

instance Show MyInt where
    show (MkMyInt i) = "MkMyInt " ++ show i

data BinTree a =    EmptyBT |
                    NodeBT a (BinTree a) (BinTree a)
                    deriving (Show)

instance Eq a => Eq (BinTree a) where 
    (==) (EmptyBT)         (EmptyBT)            = True
    (==) _                 (EmptyBT)            = False 
    (==) (EmptyBT)         _                    = False 
    (==) (NodeBT n1 t1 t2) (NodeBT n2 s1 s2)    = n1 == n2 && t1 == s1 && t2 == s2

