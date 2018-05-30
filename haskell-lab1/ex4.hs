sqr :: Double -> Double
sqr x = x * x

vec2DLen :: (Double, Double) -> Double
vec2DLen (x,y) = sqrt (sqr x + sqr y)

vec3DLen :: (Double, Double, Double) -> Double
vec3DLen (x, y, z) = sqrt (x^2 + y^2 + z^2)

swap :: (Int, Char) -> (Char, Int)
swap (a, b) = (b, a)

threeEqual :: (Int, Int, Int) -> Bool
threeEqual (x, y, z) = if (x == y && y == z) then True
                       else False