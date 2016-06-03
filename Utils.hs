-- utility functions for Euler

module Utils (
    primes
  , primes'
) where

import Data.List.Ordered

primes :: Integer -> [Integer]
primes m = sieve [2..m]
    where
        sieve [] = []
        sieve (p:xs) = p : sieve (xs `minus` [p, p+p..])


primes' :: [Integer]
primes' = 2 : filter (null . tail . fasterprimes) [3,5..]

fasterprimes :: Integer -> [Integer]
fasterprimes n = factor n primes'
    where
        factor n (p:ps)
          | p*p > n        = [n]
          | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
          | otherwise      = factor n ps
