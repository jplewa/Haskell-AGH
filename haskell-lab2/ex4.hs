import Data.Char

isPalindrome :: [Char] -> Bool
isPalindrome [] = True
isPalindrome s = if (length s == 1) then True 
    else if (head s /= last s) then False
    else isPalindrome (init(tail s))

getElemAtIdx :: Int -> [a] -> a
getElemAtIdx x xs = head(drop x xs)

    capitalize :: [Char] -> [Char]
    capitalize w = toUpper (head w) : tail w
