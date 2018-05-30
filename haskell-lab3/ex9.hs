sumWith :: Num a => (a -> a) -> [a] -> a
sumWith g []        = 0
sumWith g (x:xs)    = g x + sumWith g xs

prodWith :: Num a => (a -> a) -> [a] -> a
prodWith g []       = 1
prodWith g (x:xs)   = g x * prodWith g xs

sumWith' :: Num a => (a -> a) -> [a] -> a
sumWith' = go 0
    where   go acc g []     = acc
            go acc g (x:xs) = go (acc + g x) g xs

prodWith' :: Num a => (a -> a) -> [a] -> a
prodWith' = go 1
    where   go acc _ []     = acc
            go acc g (x:xs) = go (acc * g x) g xs
            
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ z [] = z
foldr' f z (x:xs) =  f x (foldr' f z xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z [] = z
foldl' f z (x:xs) = foldl' f (f z x) xs

sumWith'' g = foldr' (\x acc -> g x + acc) 0
sumWith''' g = foldl' (\acc x -> g x + acc) 0

prodWith'' g = foldr' (\x acc -> g x * acc) 1
prodWith''' g = foldl' (\acc x -> g x * acc) 1
