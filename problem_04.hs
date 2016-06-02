-- Problem 4

isPalindrome :: Int -> Bool
isPalindrome n = let s = show n
                  in s == reverse s

problem_4 :: Int
problem_4 = maximum $ filter isPalindrome [x*y | x <- [999, 998..100], y <- [x, x-1..100]]

