module Year2016.Day09
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import FlatParse.Basic

data Marker = Marker { dataLen :: Int
                     , repeatCount :: Int
                     }

parseMarker :: Parser () Marker
parseMarker = do
  dl <- $(char '(') *> anyAsciiDecimalInt
  rc <- $(char 'x') *> anyAsciiDecimalInt <* $(char ')')
  pure $ Marker dl rc

decompressedLength :: (ByteString -> Int) -> ByteString -> Int
decompressedLength f input
    | B.null input = 0
    | otherwise =
        case (runParser parseMarker input) of
          (OK marker input') -> let (repeatedChars, rest) = B.splitAt (dataLen marker) input'
                                in repeatCount marker * f repeatedChars + decompressedLength f rest
          Fail -> 1 + decompressedLength f (B.tail input)
          _ -> error "unreachable"

part1 :: ByteString -> Int
part1 = decompressedLength B.length

part2 :: ByteString -> Int
part2 = decompressedLength part2
