sqr x = x^2

funcFactory n = case n of
        1 -> id
        2 -> sqr
        3 -> (^3)
        4 -> \x -> x^4
        5 -> intFunc
        _ -> const n

        where
            intFunc x = x^5

-- wyrażenie list comprehension -> map, fold
-- collection pipeline

expApproxUpTo :: Int -> Double -> Double
expApproxUpTo 0 _ = 1
expApproxUpTo n x = (x^n) / (fromIntegral(factorial n)) + expApproxUpTo (n-1) x
                    where   factorial 0 = 1
                            factorial m = m*factorial(m-1)

dfr :: (Double -> Double) -> Double -> (Double -> Double)
dfr f h = \x -> (f (x + h) - f (x)) / h

dfc :: (Double -> Double) -> Double -> (Double -> Double)
dfc f h = \x -> (f (x + h) - f (x-h)) / (2*h)

