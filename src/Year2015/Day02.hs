module Year2015.Day02
    ( part1
    , part2
    ) where

import Data.Text (Text)
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

process :: (Num a) => ((a, a, a) -> a) -> Text -> a
process f xs = sum $ map f $ fromJust $ parseMaybe @() (line `sepBy` char '\n') xs
    where line = (,,) <$> decimal <* char 'x' <*> decimal <* char 'x' <*> decimal

part1 :: Text -> Int
part1 = process $ \(l, w, h) -> 2*l*w + 2*l*h + 2*w*h + minimum [l*w, l*h, w*h]

part2 :: Text -> Int
part2 = process $ \(l, w, h) -> l*w*h + 2 * minimum [l+w, l+h, w+h]
