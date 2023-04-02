{-# LANGUAGE OverloadedStrings #-}

module Year2016.Day04
    ( part1
    , part2
    ) where

import Control.Arrow ((&&&))
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.List (sort)
import FlatParse.Basic hiding (take)

data Room = Room { name :: ByteString
                 , sid :: Int
                 }

getRooms :: ByteString -> [Room]
getRooms = parse . B.lines
    where word = byteStringOf $ some $ satisfy isLower
          parseRoom = do
            room <- Room <$> (B.intercalate "-" <$> some (word <* $(char '-'))) <*> anyAsciiDecimalInt
            checksum <- $(char '[') *> word <* $(char ']')
            guard $ (== checksum) $ B.pack $ take 5 $ map snd $ sort
                      $ map ((negate . length &&& head) . B.unpack) $ B.group
                      $ B.sort $ B.filter (/= '-') $ name room
            pure room
          parse [] = []
          parse (x:xs) = case runParser parseRoom x of
                           OK res _ -> res : parse xs
                           Fail -> parse xs
                           _ -> error "unreachable"

part1 :: ByteString -> Int
part1 = sum . map sid . getRooms

rotate :: Int -> Char -> Char
rotate _ '-' = ' '
rotate n c = chr $ (ord c - n - 97) `mod` 26 + 97

isNorthPole :: Room -> Bool
isNorthPole (Room en si) = B.map (rotate si) "northpole" `B.isInfixOf` en

part2 :: ByteString -> Int
part2 = sid . head . filter isNorthPole . getRooms
