class Mappable t where
    fMap :: (a -> b) -> t a -> t b

data Vec3D a = Vec3D {x :: a, y :: a, z :: a} deriving Show

instance Mappable Vec3D where
    fMap f (Vec3D x y z) = Vec3D (f x) (f y) (f z)

newtype Pair a = Pair (a,a) deriving Show

instance Mappable Pair where
    fMap f (Pair (x,y)) = Pair ((f x), (f y))

data BinTree a =    EmptyBT |
                    NodeBT a (BinTree a) (BinTree a)
                    deriving (Show)

instance Mappable BinTree where
    fMap _ (EmptyBT)        = EmptyBT
    fMap f (NodeBT n t1 t2) = NodeBT (f n) (fMap f t1) (fMap f t2)

instance Mappable Maybe where
    fMap _ Nothing  = Nothing
    fMap f (Just x) = Just (f x)

instance Mappable (Either a) where
    fMap f (Left x)     = Left x
    fMap f (Right y)    = Right (f y)

--instance Mappable ((->) a) where
--    fMap f g =

data MyType = C1 Int | C2 Double Bool

instance Eq MyType where
    (==) (C1 i1) (C1 i2)        = i1 == i2
    (==) (C2 d1 b1) (C2 d2 b2)  = d1 == d2 && b1 == b2
    (==) _ _                    = False
