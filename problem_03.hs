module Euler03 where

main :: IO ()
main = print $ last . factor $ 600851475143

factor :: Integer -> [Integer]
factor = fac' (2:[3,5..]) where
    fac' (p:ps) n
      | p*p > n         = [n]
      | n `mod` p == 0   = p : fac' (p:ps) (n `div` p)
      | otherwise       = fac' ps n
