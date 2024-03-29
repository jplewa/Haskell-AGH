absInt :: Int -> Int
absInt n | n >= 0 = n
         | otherwise = -n

sgn :: Int -> Int
sgn n | n == 0 = 0
      | (absInt n) /= n = -1
      | otherwise = 1

min3Int :: (Int, Int, Int) -> Int
min3Int (a, b, c) | a <= b && a <= c = a
                  | b < c = b
                  | otherwise = c