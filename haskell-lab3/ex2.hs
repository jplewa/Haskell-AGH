import Data.Char
import Data.List

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f []        = 0
sumWith f (x:xs)    = (f x) + sumWith f xs

sumSqrt :: Floating a => [a] -> a
sumSqrt = sumWith (\x -> sqrt x)

sum'' :: Num a => [a] -> a
sum'' = sumWith (\x -> x)

sumSqr' :: Num a => [a] -> a
sumSqr' = sumWith (\x -> x*x)

sumCube' :: Num a => [a] -> a
sumCube' = sumWith (\x -> x*x*x)

sumAbs' :: Num a => [a] -> a
sumAbs' = sumWith (\x -> abs x)

listLength :: Num a => [a] -> a
listLength = sumWith (\x -> 1)

k1 :: Num a => a -> a 
k1 = (*2)

-- let k2 = [i^2 | i <- [1..10], i `mod` 2 == 0]

k3 :: Num a => [a] -> a
k3 = loop 1
    where   loop acc []     = acc
            loop acc (x:xs) = loop (acc*x) xs

k4 :: Num a => a -> a
k4 = (+) 3

-- let k5 = [i^3 | i <- [1..10], i `mod` 2 == 1]
-- let k5 = [i^3 | i <- [1, 3..10]]

selectEven :: Integral a => [a] -> [a]
selectEven = loop []
        where   loop s [] = s 
                loop s (x:xs)   | (even x)  = loop (s ++ [x]) xs
                                | otherwise = loop s xs

concat' :: [[a]] -> [a]
concat' = foldr (++) []

capitalize :: [Char] -> [Char]
capitalize [] = []
capitalize (x:xs) = toUpper x : (map toLower xs)

fS :: [Char] -> [Char]
fS s =  foldr1 (\w s -> w ++ " " ++ s) .
        map capitalize .
        filter (\x -> length x > 1) $
        words s

fS2 :: [Char] -> [Char]
fS2 s = foldr1 (\w s -> w ++ " " ++ s) (map capitalize (filter (\x -> length x > 1) (words s)))

prod':: Num a => [a] -> a
prod' [] = 1
prod' (x:xs) = x * prod' xs

prodWith :: Num a => (a -> a) -> [a] -> a
prodWith _ [] = 1
prodWith f (x:xs) = f x * prodWith f xs

prodCube s = prodWith (^3) s

f :: Num a => (a -> a) -> (a -> a -> a) -> a -> [a] -> a
f _ _ a [] = a
f g h a s  = foldl (\acc x -> h (g x) acc) a s