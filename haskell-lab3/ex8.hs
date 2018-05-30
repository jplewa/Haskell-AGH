import Data.Char

doubleElems :: Num a => [a] -> [a]
doubleElems []      = []
doubleElems (x:xs)  = 2*x : doubleElems xs

squareElems :: Num a => [a] -> [a]
squareElems []      = []
squareElems (x:xs)  = x^2 : squareElems xs

lowerCase :: [Char] -> [Char]
lowerCase []        = []
lowerCase (x:xs)    = toLower x : lowerCase xs

map':: (a -> b) -> [a] -> [b]
map' _ []       = []
map' f (x:xs)   = f x : map' f xs

doubleElems' :: Num a => [a] -> [a]
doubleElems' = map' (*2)

squareElems' :: Num a => [a] -> [a]
squareElems' = map' (^2)

lowerCase' :: [Char] -> [Char]
lowerCase' = map' toLower

doubleElems'' :: Num a => [a] -> [a]
doubleElems'' [] = []
doubleElems'' s  = [i*2 | i <- s]

squareElems'' :: Num a => [a] -> [a]
squareElems'' [] = []
squareElems'' s  = [i^2 | i <- s]

evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt _ [] = []
evalFuncListAt x (f:fs) = f x : evalFuncListAt x fs

evalFuncListAt' :: a -> [a -> b] -> [b]
evalFuncListAt' _ [] = []
evalFuncListAt' x f = [i x| i <- f]
