triangle :: Integer -> Int
triangle x = length ([(a,b,c) | a <- [1..x], b <- [a..x], c <-[b..x], a^2+b^2 == c^2])

isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []

isPrime2 :: Integral t => t -> Bool
isPrime2 n = if (n < 2) then False
            else if ((n == 2) || (n == 3)) then True
            else if n `mod` 2 == 0 then False
            else if n `mod` 3 == 0 then False
            else [i | i <- [j | j <- [5..n], j*j <= n, j `mod` 2 /= 0], n `mod` i == 0] == []

countPrimes :: Integral a => a -> Int
countPrimes x = length [i | i <- [2..x], isPrime2 i]

primes :: [Int]
primes = eratoSieve [2..]
    where
        eratoSieve :: [Int] -> [Int]
        eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]

isPrime3 :: Int -> Bool
isPrime3 n = any (== n) (takeWhile (<=n) primes)

countPrimes2 :: Int -> Int
countPrimes2 n = length(takeWhile (<=n) primes)

allEqual :: Eq a => [a] -> Bool
allEqual xs =   if (tail xs == []) then True
                else if (head xs /= head (tail xs)) then False
                else allEqual (tail xs)