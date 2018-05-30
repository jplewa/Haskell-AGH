-- konwencja: prefix 'Mk' dla konstruktora
data CartInt2DVec = MkCartInt2DVec Int Int 
                    deriving (Show) 

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y

type X = Int
type Y = Int

data CartInt2DVec2 = MkCartInt2DVec2 X Y 
                    deriving (Show) 

xCoord2 :: CartInt2DVec2 -> X
xCoord2 (MkCartInt2DVec2 x _) = x

yCoord2 :: CartInt2DVec2 -> Y
yCoord2 (MkCartInt2DVec2 _ y) = y



data Cart2DVec' a = MkCart2DVec' a a 

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y


data Cart2DVec'' a = MkCart2DVec''
    {
        x1 :: a,
        y1 :: a        
    } deriving (Show)

--xCoord'' :: Cart2DVec'' a -> a
--xCoord'' MkCart2DVec'' {x = xVal, y = _} = xVal

--yCoord'' :: Cart2DVec'' a -> a
--yCoord'' MkCart2DVec'' {y = yVal, x = _} = yVal

-- sum type example (two constructors)

data List a =   EmptyL | Cons a (List a)
                deriving (Show)

head' :: List a -> a
head' EmptyL        = error "head': the empty list has no head!"
head' (Cons x xs)   = x

-- enum type example (special case of sum type)
data ThreeColors =  Blue    |
                    White   |
                    Red'

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red' = "Irene Jacob"

data Cart3DVec a = Cart3DVec a a a

xCoord3D :: Cart3DVec a -> a
xCoord3D (Cart3DVec x _ _) = x

yCoord3D :: Cart3DVec a -> a
yCoord3D (Cart3DVec _ y _) = y

zCoord3D :: Cart3DVec a -> a
zCoord3D (Cart3DVec _ _ z) = z


data Cart3DVec' a = Cart3DVec'
    {
        x :: a,
        y :: a,
        z :: a
    } deriving (Show)

data Shape =    Circle Float |
                Rectangle Float Float
                deriving (Show)

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rectangle a b) = a*b

data Tree a =   EmptyT  |
                Node a (Tree a) (Tree a)
                deriving (Show)

rootValue :: Tree a -> a
rootValue EmptyT        = error "rootValue: the tree is empty!"
rootValue (Node a _ _)  = a

data TrafficLights =    Green   |
                        Yellow  |
                        Red

data ActTrafficLights = ActTrafficLights TrafficLights (Maybe TrafficLights)
                        
actionFor :: ActTrafficLights -> String
actionFor (ActTrafficLights Green Nothing)      = "Keep driving!"
actionFor (ActTrafficLights Yellow Nothing)     = "Slow down!"
actionFor (ActTrafficLights Red (Just Yellow))  = "Stop daydreaming!"
actionFor (ActTrafficLights Red Nothing)        = "Stop!"
actionFor (ActTrafficLights _ _)                = error "TrafficLights: um, something's wrong?"

data Actions =  Continue       |
                Slower         |
                Stop
                deriving (Show)

enumActionFor :: TrafficLights -> Actions
enumActionFor Green     = Continue
enumActionFor Yellow    = Slower
enumActionFor Red       = Stop