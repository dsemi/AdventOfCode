module Year2015.Day02
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import FlatParse.Basic

process :: (Int -> Int -> Int -> Int) -> ByteString -> Int
process f input = case runParser (some line) input of
                    OK ns _ -> sum ns
                    _ -> error "unreachable"
    where line = f <$> anyAsciiDecimalInt <* $(char 'x')
                 <*> anyAsciiDecimalInt <* $(char 'x')
                 <*> anyAsciiDecimalInt <* optional_ $(char '\n')

part1 :: ByteString -> Int
part1 = process $ \l w h -> 2*l*w + 2*l*h + 2*w*h + minimum [l*w, l*h, w*h]

part2 :: ByteString -> Int
part2 = process $ \l w h -> l*w*h + 2 * minimum [l+w, l+h, w+h]
