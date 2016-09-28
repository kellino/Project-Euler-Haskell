-- | a problem solved with combinatorics

choose :: Integer -> Integer -> Integer
n `choose` k = fac n `div` (fac k * fac (n - k))
    where fac x = product [1..x]

main :: IO ()
main = print $ 40 `choose` 20
