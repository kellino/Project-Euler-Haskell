-- utility functions for Euler

module Utils
    ( primes
    , modexp
    , primeFactors
    , isPrime
    , properDivisors
    )
where

import Data.Bits
import Data.List

minus :: Ord a => [a] -> [a] -> [a]
minus []       ys       = ys
minus xs       []       = xs
minus (x : xs) (y : ys) = case compare x y of
    LT -> x : minus xs (y : ys)
    EQ -> minus xs ys
    GT -> minus (x : xs) ys

primes :: [Integer]
primes = 2 : sieve [3,5..]
    where sieve [] = []
          sieve (x:xs) = x : sieve (xs `minus` [x*x, x*x+x..])

isPrime :: Integer -> Bool
isPrime n = n > 1 && foldr (\p r -> p * p > n || ((n `rem` p) == 0 && r)) True primes

modexp :: (Num t, Bits t, Integral a) => a -> t -> a -> a
modexp _ _ 1 = 0
modexp _ 0 _ = 1
modexp b e m = 
    let c = if testBit e 0 then b `mod` m else 1
     in (c * modexp (b*b `rem` m) (shiftR e 1) m) `mod` m

squareRoot :: Integer -> Integer
squareRoot = ceiling . sqrt . fromIntegral

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n = foldr (\p v -> if n `rem` p == 0 then p:v else v) [] (genericTake (squareRoot n) primes)

properDivisors :: Integer -> [Integer]
properDivisors n = 
    let start = foldr (\d v -> if n `rem` d == 0 then d:v else v) [] [2..squareRoot n]
        end   = foldr (\d v -> if n `rem` d == 0 then (n `div` d):v else v) [] start
     in 1 : start ++ reverse end
