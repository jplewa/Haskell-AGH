f1 :: (Floating a, Num a, Eq a) => a -> a
f1 x = case (x) of
        1 -> 1
        2 -> y
            where y = 2 * sqrt 2 * sin 2
        _ -> 0