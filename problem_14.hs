import Control.Monad (ap)
import Data.List (maximumBy)
import Data.Ord (comparing)

collatz :: Int -> [Int]
collatz n
    | n == 1    = [1]
    | n `mod` 2 == 0 = n : collatz (n `div` 2)
    | otherwise = n : collatz (3 * n + 1)

main :: IO ()
main = print $ maximumBy (comparing snd) $ map (ap (,) (length . collatz)) [2..999999]
