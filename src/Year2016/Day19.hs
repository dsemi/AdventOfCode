{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Year2016.Day19
    ( part1
    , part2
    ) where


josephus :: Int -> Int
josephus n = 1 + 2 * (n - 2 ^ largestPowerOf2)
    where largestPowerOf2 = floor $ logBase 2 $ fromIntegral n

part1 :: Int -> Int
part1 = josephus

weirdJosephus :: Int -> Int
weirdJosephus n = let p3 = 3 ^ largestPowerOf3 n
                      ans  = n - p3
                      ans' = ans + max 0 (ans - p3)
                  in if ans' == 0 then p3 else ans'
    where largestPowerOf3 = floor . logBase 3 . fromIntegral

part2 :: Int -> Int
part2 = weirdJosephus
