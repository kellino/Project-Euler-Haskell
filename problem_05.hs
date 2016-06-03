-- Problem 5, using least common multiple
-- no point checking 1 and 20, so let's not do the calculation

main :: IO ()
main = print $ foldr1 lcm [2..19] 
