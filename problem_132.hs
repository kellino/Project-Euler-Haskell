module Main where

import Utils

euler :: Int -> [Int]
euler k = foldr (\p v -> if modexp 10 k (9*p) == 1 then p:v else v) [] primes

main :: IO ()
main = print . sum . take 40 . euler $ 10^9
