{-# LANGUAGE BangPatterns #-}

fib :: (Num a, Eq a) => a -> a
fib n = if n == 0 || n == 1 then n
        else fib (n-2) + fib (n-1)

fib2 :: Integral a => Int -> a
fib2 n = last (take (n+1) fibs)
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
        
prod' :: Fractional a => [a] -> a
prod' [] = 1
prod' (x:xs) = x * prod' xs

prod'2 :: Fractional a => [a] -> a
prod'2 =    loop 1
            where   loop acc []  = acc
                    loop acc (x:xs) = loop (acc*x) xs

sum' :: Fractional a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

sum'2 :: Num a => [a] -> a
sum'2 xs =      loop 0 xs
                where   loop acc [] = acc
                        loop acc (x:xs) = loop (acc+x) xs

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
        where   loop acc [] = acc
                loop acc (x:xs) = loop (acc+x) xs

sum'4 :: Num a => [a] -> a
sum'4 = loop 0
        where   loop !acc []            = acc
                loop !acc (x:xs)        = loop (acc+x) xs

length' :: Fractional t => [a] -> t
length' [] = 0
length' (x:xs) = 1 + length' xs

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || or' xs

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs) = (n == x) || elem' n xs

doubleAll :: Num t => [t] -> [t]
doubleAll x = map (\ e -> e*2) x

doubleAll':: Num t => [t] -> [t]
doubleAll' x = zipWith (*) x [2 | i <- [1..(length x)]]

squareAll :: Num t => [t] -> [t]
squareAll x = map (\e -> e^2) x

meanA :: Floating a => [a] -> a
meanA x = (sum' x) / (length' x)

meanG :: Floating a => [a] -> a
meanG x = (prod' x) ** (1 / length' x)

mean :: Floating a => [a] -> (a, a)
mean x = (meanA x, meanG x)