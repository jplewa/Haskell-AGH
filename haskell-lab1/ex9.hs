roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = ((-b - d)/e, (-b + d)/e)
    where d = sqrt (b*b - 4*a*c)
          e = 2*a

unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (a, b) = (c, d)
    where c = if (a == 0) then 0
              else if (a < 0) then -1
                   else 1
          d = if (b == 0) then 0
              else if (b < 0) then -1
                   else 1      

unitVec2D' :: (Double, Double) -> (Double, Double)
unitVec2D' (a, b) = (c, d)
    where c | (a == 0) = 0
            | (a < 0) = -1
            | otherwise = 1
          d | (b == 0) = 0
            | (b < 0) = -1
            | otherwise = 1