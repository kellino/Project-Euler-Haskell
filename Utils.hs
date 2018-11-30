-- utility functions for Euler

module Utils
    ( primes
    , modexp
    )
where

import Data.Bits

minus :: Ord a => [a] -> [a] -> [a]
minus []       ys       = ys
minus xs       []       = xs
minus (x : xs) (y : ys) = case compare x y of
    LT -> x : minus xs (y : ys)
    EQ -> minus xs ys
    GT -> minus (x : xs) ys

primes :: [Int]
primes = sieve [2..]
    where sieve [] = []
          sieve (x:xs) = x : sieve (xs `minus` [x*x, x*x+x..])

modexp :: (Num t, Bits t, Integral a) => a -> t -> a -> a
modexp _ _ 1 = 0
modexp _ 0 _ = 1
modexp b e m = 
    let c = if testBit e 0 then b `mod` m else 1
     in (c * modexp (b*b `rem` m) (shiftR e 1) m) `mod` m
