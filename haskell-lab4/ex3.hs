data BinIntTree =   EmptyIntBT |
                    IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n t1 t2) = n + sumBinIntTree t1 + sumBinIntTree t2

data BinTree a =    EmptyBT |
                    NodeBT a (BinTree a) (BinTree a)
                    deriving (Show)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n t1 t2) = n + sumBinTree t1 + sumBinTree t2

depthOfBT :: BinTree a -> Int
depthOfBT EmptyBT = 0
depthOfBT (NodeBT n t1 t2) =    if (depthOfBT t1) > (depthOfBT t2)
            then (depthOfBT t1) + 1
            else (depthOfBT t1) + 1

-- inorder
flattenBT :: BinTree a -> [a]
flattenBT EmptyBT = []
flattenBT (NodeBT n t1 t2) = (flattenBT t1) ++ [n] ++ (flattenBT t2)

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT f EmptyBT = EmptyBT
mapBT f (NodeBT n t1 t2) = (NodeBT (f n) (mapBT f t1) (mapBT f t2))

insert :: Ord a => a -> BinTree a -> BinTree a
insert x EmptyBT = NodeBT x EmptyBT EmptyBT
insert x (NodeBT n t1 t2) = if x < n
        then NodeBT n (insert x t1) t2
        else NodeBT n t1 (insert x t2)

list2BST :: Ord a => [a] -> BinTree a
list2BST [] = EmptyBT
list2BST (x:xs) = insert x (list2BST xs) 

occurs :: Eq a => a -> BinTree a -> Int
occurs _ EmptyBT = 0
occurs x (NodeBT n t1 t2) = if n == x
        then 1 + (occurs x t1) + (occurs x t2)
        else (occurs x t1) + (occurs x t2)

elemOf :: Eq a => a -> BinTree a -> Bool
elemOf _ EmptyBT = False
elemOf x (NodeBT n t1 t2) = x == n || elemOf x t1 || elemOf x t2

reflect :: BinTree a -> BinTree a
reflect EmptyBT = EmptyBT
reflect (NodeBT n t1 t2) = NodeBT n (reflect t2) (reflect t1)

minElemOf :: Ord a => BinTree a -> a
minElemOf EmptyBT = error "minElemOf: the tree is empty!"
minElemOf (NodeBT n t1 t2) = loop n (NodeBT n t1 t2)
         where  loop acc EmptyBT = acc
                loop acc (NodeBT n t1 t2) =
                    let minL = loop acc t1
                        minR = loop acc t2
                    in  if acc < n && acc < minL && acc < minR then acc
                        else if n < acc && n < minL && n < minR then n
                        else if minL < acc && minL < minR && minL < n then minL
                        else minR

foldBinTree :: (a -> b -> b -> b) -> b -> BinTree a -> b
foldBinTree _ x EmptyBT = x
foldBinTree f x (NodeBT n t1 t2) = f n (foldBinTree f x t1) (foldBinTree f x t2)
-- *Main> foldBinTree (\x y z -> x*y*z) 1 $ list2BST [1,2,3]
-- 6

mapBT' :: (a -> b) -> BinTree a -> BinTree b
mapBT' _ EmptyBT = EmptyBT
mapBT' f (NodeBT n t1 t2) = NodeBT (foldBinTree f (NodeBT n (EmptyBT) (EmptyBT))) (mapBT' f t1) (mapBT' f t2)

zipBT :: (a -> b -> c) -> BinTree a -> BinTree b -> BinTree c
zipBT _ _ EmptyBT = EmptyBT
zipBT _ EmptyBT _ = EmptyBT
zipBT f (NodeBT n t1 t2) (NodeBT m s1 s2) = NodeBT (f n m) (zipBT f t1 s1) (zipBT f t2 s2)

data GTree a =  Leaf a |
                GNode [GTree a]
                deriving Show

sumGTree :: Num a => GTree a -> a
sumGTree (Leaf n) = n
sumGTree (GNode []) = 0
sumGTree (GNode (x:xs)) = sumGTree x + sumGTree (GNode xs)

elemOfGTree :: Eq a => a -> GTree a -> Bool
elemOfGTree n (Leaf x) = if n == x then True else False
elemOfGTree n (GNode []) = False
elemOfGTree n (GNode (x:xs)) = (elemOfGTree n x) || (elemOfGTree n (GNode xs))

depthOfGTree :: GTree a -> Int
depthOfGTree (Leaf n) = 0
depthOfGTree (GNode [x]) = 1 + depthOfGTree x
depthOfGTree (GNode (x:xs)) =   if depthOfGTree x >  depthOfGTree (GNode xs)
            then 1 + depthOfGTree x
            else depthOfGTree (GNode xs)

mapGTree :: (a -> b) -> GTree a -> GTree b
mapGTree f (Leaf n) = (Leaf (f n))
mapGTree f (GNode [x]) = GNode [mapGTree f x]
mapGTree f (GNode (x:xs)) = GNode ([mapGTree f x] ++ [mapGTree f (GNode xs)])

data Expr a =   Lit a 
                | Add (Expr a) (Expr a)
                | Subtr (Expr a) (Expr a)
                | Mult (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Subtr e1 e2) = eval e1 - eval e2
eval (Mult e1 e2) = eval e1 * eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
show' (Subtr e1 e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"
show' (Mult e1 e2) = "(" ++ show' e1 ++ "*" ++ show' e2 ++ ")"