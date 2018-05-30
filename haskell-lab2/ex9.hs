qSort :: Ord a => [a] -> [a]
qSort [] = []
qSort (x:xs) =  qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
                where
                    leftPart xs = [y | y <- xs, y <= x]
                    rightPart xs = [y | y <- xs, y > x]

mSort :: Ord a => [a] -> [a]
mSort [] = []
mSort [x] = [x]
mSort x = merge (mSort (take ((length x) `div` 2) x)) (mSort (drop ((length x) `div` 2) x))
            where merge [] y = y
                  merge x [] = x
                  merge (x:xs) (y:ys)                    
                      | (x <= y)  = x : (merge xs (y:ys))
                      | otherwise = y : (merge (x:xs) ys)

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort [x] = [x]
iSort (x : xs) = insert x (iSort xs)
            where   insert a [] = [a]
                    insert a (y:ys) | a < y     = a : y : ys
                                    | otherwise = y : (insert a ys)

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = [y | y <- x] ++ [ys | ys <- concat' xs]

concat'' :: [[a]] -> [a]
concat'' [] = []
concat'' (x:xs) = x ++ (concat'' xs)

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted x =    if (head x <= (head (tail x))) then isSorted (tail x)
                else False

reverse' :: [a] -> [a]
reverse' [] = []
reverse' [x] = [x]
reverse' x = (last x) : reverse (init x)

zip' :: [a] -> [b] -> [(a,b)]
zip' [] y = []
zip' x [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])
unzip' ((x,y) : s) = (x:xs, y:ys)
                     where (xs, ys) = unzip' s

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' x y z | (null x) || (null y) || (null z)  = []
            | otherwise                         = (head x, head y, head z) : (zip3' (tail x) (tail y) (tail z))

subList :: Eq a => [a] -> [a] -> Bool
subList x y | (null x) = True
            | (null y) = False
            | ((head x) == (head y))  = subList (tail x) (tail y)
            | otherwise = subList x (tail y) 