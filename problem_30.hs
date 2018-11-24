module Main where

deconstruct :: Int -> [Int]
deconstruct n | n <= 9 = [n]
              | otherwise = n `rem` 10 : deconstruct (n `div` 10)

check :: Int -> Int -> Bool
check p x = x == sum (map (^ p) $ deconstruct x)

euler30 :: Int -> [Int] -> Int
euler30 p xs = sum $ filter (check p) xs

main :: IO ()
main = print $ euler30 5 [1000..355000]
