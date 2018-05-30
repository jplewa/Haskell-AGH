concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- list comprehension
-- concat'' :: [[a]] -> [a]

concat''' :: [[a]] -> [a]
concat''' s = foldr (++) [] s

