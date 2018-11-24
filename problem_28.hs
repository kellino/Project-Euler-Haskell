module Main where

spiral :: Integer -> Integer
spiral n = if n == 1 then 1 else 4 * n^2 - 6*(n -1) + spiral (n - 2)

main :: IO ()
main = do
    n <- getLine
    print . spiral . read $ n
