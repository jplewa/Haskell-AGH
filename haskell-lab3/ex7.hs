import Data.Char
onlyEven :: Integral a => [a] -> [a]
onlyEven [] = []
onlyEven (x:xs)
    | x `mod` 2 == 0    = x : onlyEven xs
    | otherwise         = onlyEven xs

onlyOdd :: Integral a => [a] -> [a]
onlyOdd [] = []
onlyOdd (x:xs)
    | x `mod` 2 /= 0    = x : onlyOdd xs
    | otherwise         = onlyOdd xs

onlyUpper :: [Char] -> [Char]
onlyUpper [] = []
onlyUpper (x:xs)
    | isUpper x = x : onlyUpper xs
    | otherwise = onlyUpper xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x       = x : filter' f xs
    | otherwise = filter' f xs

onlyEven' :: Integral a => [a] -> [a]
onlyEven' = filter' (\x -> even x)

onlyOdd' :: Integral a => [a] -> [a]
onlyOdd' = filter' (\x -> odd x)

onlyUpper' :: [Char] -> [Char]
onlyUpper' = filter' (\x -> isUpper x)