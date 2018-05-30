module Stack (Stack(MkStack), empty, isEmpty, push, top, pop) where
    
    empty :: Stack a
    isEmpty :: Stack a -> Bool
    push :: a -> Stack a -> Stack a
    top :: Stack a -> a
    pop :: Stack a -> (a,Stack a)
    
    newtype Stack a = MkStack [a] deriving Show
    
    empty = MkStack []
    isEmpty (MkStack s) = null s
    push x (MkStack s) = MkStack (x:s)
    top (MkStack s) = head s
    pop (MkStack (s:ss)) = (s,MkStack ss)



--data Foo a = MkFoo {value :: a, name :: String}

--instance Show (Foo a) where
--    show MkFoo {value = v, name = n} = "Name: " ++ n ++ " with " -- ++ v

--data Tree a =   Node [Tree a] a
--                | Leaf a

--maxValue :: Ord a => Tree a -> a
--maxValue (Leaf a) = a
--maxValue (Node [] n) = n
--maxValue (Node (x:xs) n) =  if n > (maxValue x) && n > (maxValue (Node xs n))
--                            then n
--                            else if (maxValue x) > (maxValue (Node xs n))
--                            then maxValue x
--                            else (maxValue (Node xs n))