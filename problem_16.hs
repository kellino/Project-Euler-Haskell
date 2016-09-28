-- | the sum of digits in 2^1000

import Data.Char

prob16 :: Int
prob16 = sum $ map digitToInt (show $ 2^1000)

main :: IO ()
main = print prob16
