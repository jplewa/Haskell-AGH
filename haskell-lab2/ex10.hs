fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

fstDivScnd :: Integral a => [a] -> Bool
fstDivScnd (x : y : _)  | x == 0            = False
                        | y `mod` x == 0    = True
fstDivScnd _                                = False

fstDivThrd :: Integral a => [a] -> Bool
fstDivThrd (x : y : z : _)  | x == 0            = False
                            | z `mod` x == 0    = True
fstDivThrd _                                    = False

