module Euler21 where

import Utils 
import Data.Maybe (mapMaybe)

amicablePair :: Integer -> Maybe (Integer, Integer)
amicablePair n = 
    let a = sum (properDivisors n) 
        b = sum (properDivisors a) 
     in if n == b && n < a then Just (n, a) else Nothing

euler21 :: Integer -> Integer
euler21 n = foldr (\(x, y) z -> x + y + z) 0 (mapMaybe amicablePair [2..n])
