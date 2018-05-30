isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = foldl (&&) True (zipWith (\a b -> a <= b) (init xs) (tail xs))

isSortedDesc :: Ord a => [a] -> Bool
isSortedDesc xs = foldl (&&) True (zipWith (\a b -> a >= b) (init xs) (tail xs))

isSorted :: Ord a => [a] -> Bool
isSorted s
    | isSortedAsc s     = True
    | isSortedDesc s    = True
    | otherwise         = False

everySecond :: [t] -> [t]
everySecond xs = zip xs [2,4..]

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' _ _ [] = []
zip3' _ [] _ = []
zip3' [] _ _ = []
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3' xs ys zs

unzip3' :: [(a,b,c)] -> ([a],[b],[c])
unzip3' [] = ([],[],[])
unzip3' ((x,y,z): xyzs) = (x:xs, y:ys, z:zs)
        where (xs, ys, zs) = unzip3' xyzs

fibs = 0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]

ones = 1 : ones

nats = 1 : map (+1) nats