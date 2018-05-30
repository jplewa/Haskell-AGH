not' :: Bool -> Bool
not' b = case b of
    True -> False
    False -> True


absInt n =
    case (n >= 0) of
        True -> n
        _    -> -n

isItTheAnswer :: String -> Bool
isItTheAnswer s =
    case (s == "Love") of 
    True -> True
    False -> False

or' :: (Bool, Bool) -> Bool
or' (a, b) =
    case (a == b && a == False) of
        True -> False
        False -> True 

and' :: (Bool, Bool) -> Bool
and' (a, b) =
    case (a == True && b == True) of
        True -> True
        _ -> False

nand' :: (Bool, Bool) -> Bool
nand' (a, b) =
    case (a /= True && b /= True) of
        True -> True
        _ -> False

xor' :: (Bool, Bool) -> Bool
xor' (a, b) =
    case (a /= b) of
        True -> True
        _ -> False

