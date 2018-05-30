-- lista funkcji, które przyjmują Double'a i zwracają Double'a
funcList :: [Double -> Double]
funcList = [\x -> (sin x)/x, \x -> log x + sqrt x + 1, \x -> (exp 1) ** x]

-- otrzymuje na wejściu argument i listę funkcji, zwraca listę wyników funkcji dla argumentu
evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt x [] = []
evalFuncListAt x (f : fs) = f x : evalFuncListAt x fs

-- krotka funkcji, które przyjmują Double'a i zwracają Double'a
displEqs :: (Double -> Double, Double -> Double)
displEqs = (\t -> 4 * t^2 + 2 * t, \t -> 3 * t^2)

--let (x_t, y_t) = (fst displEqs, snd displEqs)

funcListExt :: (Double -> Double) -> [Double -> Double] -> [Double -> Double]
funcListExt f [] = [f]
funcListExt f (x:xs) =  f : (x:xs)




--(opcjonalne) Wykorzystując zdefiniowane wcześniej funkcje dfc i d2f w konsoli GHCi obliczyć wektory reprezentujące prędkości (velocEqs) i przyspieszenia (accelEqs)
