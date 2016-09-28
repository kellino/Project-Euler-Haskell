module Main where

import Data.Numbers.Primes
import Data.List

triangleNumbers :: [Int]
triangleNumbers = scanl1 (+) [1..]

nDivisors :: Int -> Int
nDivisors n = product $ map ((+1) . length) (group (primeFactors n))

answer :: Int
answer = head $ filter ((> 500) . nDivisors) triangleNumbers

main :: IO ()
main = print answer
