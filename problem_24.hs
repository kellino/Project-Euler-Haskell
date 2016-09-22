import Data.List

-- | one possibility, but slightly slower
{-perms :: [String]-}
{-perms = sort $ map toString $ permutations [0..9]-}
    {-where toString xs = foldr1 (++) (map show xs)-}

{-main :: IO ()-}
{-main = print $ head $ drop 999999 perms-}

-- | faster just to make it a string initially
perms' :: [String]
perms' = sort $ permutations ['0'..'9']

main :: IO ()
main = print $ (!! 999999) perms'
