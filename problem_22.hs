-- Project Euler 22

import Data.List
import qualified Data.Map as Map
import Data.List.Split

cleanup :: String -> String
cleanup = map (\x -> if x == '"' || x == ',' then ' ' else x)

sumName :: String -> Int
sumName xs = sum $ sum <$> map (`Map.lookup` vals) xs

vals :: Map.Map Char Int
vals = Map.fromList $ zip ['A'..'Z'] [1..]

main :: IO ()
main = do
    names <- readFile "./data/p022_names.txt"
    print $ sum $ 
        map (\(x,y) -> x * sumName y) $ zip [1..] $ 
            dropWhile (== "") $ sort . splitOn " " . cleanup $ names
