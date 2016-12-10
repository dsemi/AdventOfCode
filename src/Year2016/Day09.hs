module Year2016.Day09
    ( part1
    , part2
    ) where

import Text.Megaparsec (char, parse)
import Text.Megaparsec.Lexer (integer)
import Text.Megaparsec.String (Parser)

data Marker = Marker { dataLen :: Int
                     , repeatCount :: Int
                     , markerLen :: Int
                     }

parseMarker :: Parser Marker
parseMarker = do
  dl <- char '(' *> int
  rc <- char 'x' *> int <* char ')'
  return . Marker dl rc . length $ format dl rc
    where int = fromInteger <$> integer
          format a b = "(" ++ show a ++ "x" ++ show b ++ ")"

decompressedLength :: (String -> Int) -> String -> Int
decompressedLength _ "" = 0
decompressedLength f input@(s:ss) =
    case (parse parseMarker "" input) of
      (Right marker) -> let input' = drop (markerLen marker) input
                            (repeatedChars, rest) = splitAt (dataLen marker) input'
                        in repeatCount marker * f repeatedChars + decompressedLength f rest
      (Left _) -> 1 + decompressedLength f ss

part1 :: String -> String
part1 = show . decompressedLength length

recursive :: String -> Int
recursive = decompressedLength recursive

part2 :: String -> String
part2 = show . decompressedLength recursive
