module Year2017.Day01
    ( part1
    , part2
    ) where

import Data.Char (digitToInt)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T


captcha :: (Int -> Int) -> Text -> Int
captcha f xs = let (end, start) = T.splitAt (f $ T.length xs) xs
               in sum [ digitToInt a
                      | (a, b) <- T.zip xs (start <> end)
                      , a == b ]

part1 :: Text -> Int
part1 = captcha (const 1)

part2 :: Text -> Int
part2 = captcha (`div` 2)
