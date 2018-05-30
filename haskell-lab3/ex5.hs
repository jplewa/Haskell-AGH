import Data.List

-- infixr 9 ???

sortDesc :: Ord a => [a] -> [a]
sortDesc = reverse.sort 

sortDesc' :: Ord a => [a] -> [a]
sortDesc' s = reverse $ sort s


-- przyjmuje dwie funkcje, które zwracają Eq i tablicę wartości
are2FunsEqAt :: Eq a => (t -> a) -> (t -> a) -> [t] -> Bool
are2FunsEqAt f g [] = True
are2FunsEqAt f g (x:xs)
    | f x /= g x    = False
    | otherwise     = are2FunsEqAt f g xs

infixl 9 >.>
(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
g >.> f = f.g

composeFunList :: [a -> a] -> (a -> a)
composeFunList [] = id
composeFunList (x:xs) = x . (composeFunList xs)

composeFunList' :: [a -> a] -> (a -> a)
composeFunList' = loop id
        where   loop f []       = f
                loop f (x:xs)   = loop (f.x) xs