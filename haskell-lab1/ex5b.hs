min2Int :: (Int, Int) -> Int
min2Int (a, b) = if a <= b then a
                 else b

min3Int :: (Int, Int, Int) -> Int
min3Int (a, b, c) = min2Int(min2Int (a, b), c)
