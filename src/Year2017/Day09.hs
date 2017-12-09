module Year2017.Day09
    ( part1
    , part2
    ) where


data Stream = Stream { score :: Int
                     , depth :: Int
                     , inGarbage :: Bool
                     , garbageCount :: Int
                     }

process :: Stream -> String -> Stream
process stream [] = stream
process stream@(Stream score depth inGarbage garbageCount) (x:xs)
    | inGarbage =
        case x of
          '!' -> process stream $ tail xs
          '>' -> process stream { inGarbage = False } xs
          _   -> process stream { garbageCount = garbageCount + 1 } xs
    | x == '}' = process stream { score = score + depth
                                , depth = depth - 1 } xs
    | x == '{' = process stream { depth = depth + 1 } xs
    | x == '<' = process stream { inGarbage = True } xs
    | otherwise = process stream xs

beginStream = Stream 0 0 False 0

part1 :: String -> Int
part1 = score . process beginStream

part2 :: String -> Int
part2 = garbageCount . process beginStream
