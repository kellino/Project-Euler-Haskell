module Problem_1 where

main :: IO ()
main = do
    print prob1
    print prob_alterative

prob1 :: Integer
prob1 = sum [x | x <- [1..999], x `mod` 3 == 0] + sum [x | x <- [1..999], x `mod` 5 == 0] - sum [15, 30..999]


geo_total :: Int -> Int -> Int
geo_total start stop =
    let n = stop ` div` start
     in (n * (n+1)) `div` 2


prob_alterative :: Int
prob_alterative = 3 * geo_total 3 999 + 5 * geo_total 5 999 - 15 * geo_total 15 999
