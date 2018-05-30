not' :: Bool -> Bool
not' True = False
not' False = True

isItTheAnswer :: String -> Bool
isItTheAnswer "Love" = True
isItTheAnswer _ = False

or' :: (Bool, Bool) -> Bool
or' (False, False) = False
or' (_, _) = True

and' :: (Bool, Bool) -> Bool
and' (True, True) = True
and' (_, _) = False

nand' :: (Bool, Bool) -> Bool
nand' (a, b) = not'(and'(a, b))

xor' :: (Bool, Bool) -> Bool
xor' (a, b) | and'(a, b) = False
           | and'((not' a), (not' b)) = False
           | otherwise = True