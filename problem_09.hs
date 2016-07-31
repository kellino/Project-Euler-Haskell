-- | Problem 9

-- very slow, naive implementation
problem9 :: Integer
problem9 = (\(x,y,z) -> x * y * z) $ head $ filter (\(x,y,z) -> x + y + z == 1000) pythag
    where pythag :: [(Integer, Integer, Integer)]
          pythag = [(x, y, z) | z <- [1..], x <- [1..z], y <- [x..z], x^2 + y^2 == z^2]


