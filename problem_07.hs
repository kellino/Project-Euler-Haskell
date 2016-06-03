import Data.List.Ordered

-- this definition of primes is taken from the haskell.org primes page
primes :: Integer -> [Integer]
primes m = sieve [2..m]
    where 
        sieve [] = []
        sieve (p:xs) = p : sieve (xs `minus` [p, p+p..])

main :: IO ()
main = print $ (!! 10000) $ primes 1000000
