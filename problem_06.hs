-- Problem 6

import Data.Function

floatDiv :: Integer -> Integer -> Double
floatDiv = (/) `on` fromIntegral

sumSquareDiff :: Fractional a => a -> a
sumSquareDiff n = ((n * (n+1)) / 2)^2 - ((n * (n+1) * (2 * n + 1)) / 6) 

main :: IO ()
main = print $ sumSquareDiff 100
