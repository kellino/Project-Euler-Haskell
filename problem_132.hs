module Main where

import Data.Bits

minus :: Ord a => [a] -> [a] -> [a]
minus [] ys = ys
minus xs [] = xs
minus (x:xs) (y:ys) = 
    case compare x y of
        LT -> x : minus xs (y:ys)
        EQ -> minus xs ys
        GT -> minus (x:xs) ys

primes :: [Int]
primes = sieve [2..]
    where sieve [] = []
          sieve (x:xs) = x : sieve (xs `minus` [x*x, x*x+x..])


modexp :: (Num t, Bits t, Integral a) => a -> t -> a -> a
modexp _ _ 1 = 0
modexp _ 0 _ = 1
modexp b e m = 
    let c = if testBit e 0 then b `mod` m else 1
     in (c * modexp (b^2 `mod` m) (shiftR e 1) m) `mod` m

euler :: Int -> [Int]
euler k = foldr (\p v -> if modexp 10 k (9*p) == 1 then p:v else v) [] primes

main :: IO ()
main = print . sum . take 40 . euler $ 10^9
