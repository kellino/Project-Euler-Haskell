-- Problem 8
-- this code is pretty ugly, but it's fast enough and gives the correct answer, so it'll do!

import Data.Char

main :: IO ()
main = do
    bignum <- readFile "./data/prob8_text"
    print $ maximum $ map getProduct (getLength 13 $ eatString 12 $ joinNum bignum)

getLength :: Int -> [String] -> [String]
getLength n xs = filter (\x -> length x == n) xs

joinNum :: String -> String
joinNum = filter (/= '\n')

getProduct :: String -> Int
getProduct = product . map digitToInt 

eatString n xs = 
    go xs []
        where go [] acc = acc
              go (x:xs) acc = go xs ((x : take n xs) : acc)
