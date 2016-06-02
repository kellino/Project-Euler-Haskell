-- Problem 13

main :: IO ()
main = do
    nums <- readFile "./nums_for_13"
    print $ take 10 $ (show . sumNums) $ words nums

sumNums :: [String] -> Integer
sumNums nums = sum $ map read nums
