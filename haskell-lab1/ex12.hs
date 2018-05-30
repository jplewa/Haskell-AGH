signum1 :: (Num a, Ord a, Integral b) => a -> b
signum1 x = if x > 0 then 1 else if x < 0 then -1 else 0 

signum2 :: (Num a, Ord a, Integral b) => a -> b
signum2 0 = 0
signum2 x   | x < 0     = -1
            | x > 0     = 1

signum3 :: (Num a, Ord a, Integral b) => a -> b
signum3 0 = 0
signum3 x = case (x < 0) of
            True -> (-1)
            _ -> 1

absInt1 :: Integer -> Integer
absInt1 x   | x >= 0    = x
            | x < 0    = (-1)*x

absInt2 :: Integer -> Integer
absInt2 x = case (x >= 0) of
            True -> x
            _ -> (-1)*x