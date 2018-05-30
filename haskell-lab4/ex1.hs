polarToCartesian :: Floating a => (a, a) -> (a, a)
polarToCartesian (r, phi) = (r * cos phi, r * sin phi)

type CartesianCoord' a = (a, a)
type PolarCoord' a = (a, a)

polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r, phi) = (r * cos phi, r * sin phi)

newtype CartesianCoord'' a = MkCartesianCoord'' (a, a) deriving (Show)
newtype PolarCoord'' a = MkPolarCoord'' (a, a) deriving (Show)

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian'' (MkPolarCoord'' (r, phi)) = MkCartesianCoord''(r * cos phi, r * sin phi)

-- (opcjonalne) Napisać odpowiedniki powyższych typów i funkcji dla układów: cylindrycznego i sferycznego

personInfoToString :: (String, String, String) -> String
personInfoToString (nm,snm,addr) = 
    "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr

type Name' = String
type Surname' = String
type Address' = String
type PersonInfo' = (Name', Surname', Address')
type PersonInfoToStringType' = PersonInfo' -> String

-- oraz napisać i sprawdzić działanie funkcji
personInfoToString' :: PersonInfoToStringType'
personInfoToString' (nm,snm,addr) = 
    "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr

--(opcjonalne) Wykorzystując newtype napisać funkcję

newtype Name'' a = MkName'' a deriving (Show)
newtype Surname'' a = MkSurname'' a deriving (Show)
newtype Address'' a = MkAddress'' a deriving (Show)

type PersonInfo'' = (Name'' String, Surname'' String, Address'' String)

personInfoToString'' :: PersonInfo'' -> String
personInfoToString'' (MkName'' nm, MkSurname'' snm, MkAddress'' addr) =
    "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr
    

--która będzie bardziej ‘odporna’ na błędne dane wejściowe; sprawdzić działanie funkcji

