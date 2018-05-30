f1 :: Real a => a -> a
f1 = \x -> x-2

f2 :: (Floating a) => (a,a) -> a
f2 = \(x,y) -> sqrt (x*x + y*y)

f2' :: (Floating a) => a -> a -> a
f2' = \x y -> sqrt (x*x + y*y)

f3 :: (Integral a, Floating b) => a -> a -> a -> b
f3 = \x y z -> sqrt (fromIntegral (x*x + y*y + z*z))

f4 :: Num a => a -> a
f4 = \x -> 2*x

f5 :: Num a => a -> a
f5 = \x -> x*2

f6 :: Floating a => a -> a
f6 = \x -> (2 ** x)

f7 :: Num a => a -> a
f7 = \x -> x^2

f8 :: Fractional a => a -> a
f8 = \x -> (2/x)

f9 :: Fractional a => a -> a
f9 = \x -> (x/3)

f10 :: Num a => a -> a
f10 = \x -> (4-x)

sqrtL :: Num a => a -> a
sqrtL = \x -> x^2

absL :: (Num a, Ord a) => a -> a
absL = \x -> if x>=0 then x else (-1)*x

f11 :: (Num a, Integral a) => a -> Bool
f11 = \x -> if (x `mod` 2 == 0) then True else False

f12 :: Floating a => a -> a
f12 = \x -> let y = sqrt x in 2 * y^3 * (y+1)

f13 :: (Num a, Eq a) => a -> a
f13 = \x -> if (x == 1) then 3 else 0