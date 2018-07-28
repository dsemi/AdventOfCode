module Year2016.Day09
    ( part1
    , part2
    ) where

import Utils

import Text.Megaparsec (parse)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

data Marker = Marker { dataLen :: Int
                     , repeatCount :: Int
                     , markerLen :: Int
                     }

parseMarker :: Parser Marker
parseMarker = do
  dl <- char '(' *> int
  rc <- char 'x' *> int <* char ')'
  return . Marker dl rc . length $ format dl rc
    where int = fromInteger <$> decimal
          format a b = "(" ++ show a ++ "x" ++ show b ++ ")"

decompressedLength :: (String -> Int) -> String -> Int
decompressedLength _ "" = 0
decompressedLength f input@(_:ss) =
    case (parse parseMarker "" input) of
      (Right marker) -> let input' = drop (markerLen marker) input
                            (repeatedChars, rest) = splitAt (dataLen marker) input'
                        in repeatCount marker * f repeatedChars + decompressedLength f rest
      (Left _) -> 1 + decompressedLength f ss

part1 :: String -> Int
part1 = decompressedLength length

part2 :: String -> Int
part2 = decompressedLength part2
