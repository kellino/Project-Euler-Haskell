module Problem2 where

main :: IO ()
main = 
    print prob_2


fibs' :: [Integer]
fibs' = 0 : 1 : zipWith (+) fibs' (tail fibs')

prob_2 :: Integer
prob_2 = sum $ filter even $ takeWhile (< 4000000) fibs'
