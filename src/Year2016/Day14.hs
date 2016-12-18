module Year2016.Day14
    ( part1
    , part2
    ) where

import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Either (rights)
import Data.Maybe (listToMaybe)
import Data.Sequence ((|>), (><), Seq, ViewR(..))
import qualified Data.Sequence as S
import qualified Crypto.Hash.MD5 as MD5
import Data.Monoid ((<>))
import Text.Megaparsec (anyChar, char, parse)
import Text.Megaparsec.ByteString (Parser)


hash :: ByteString -> ByteString
hash = encode . MD5.hash

findAll :: Parser a -> ByteString -> [a]
findAll parser = rights . map (parse parser "") . init. B.tails

find3 :: Parser Char
find3 = anyChar >>= char >>= char

find :: ByteString -> (ByteString -> ByteString) -> Int -> Int -> Seq Int
find seed hash num n = go S.empty S.empty [0..]
    where go :: Seq Int -> Seq (Int, Char) -> [Int] -> Seq Int
          go c pot (x:xs)
              | S.length c >= num = S.take num c
              | otherwise =
                  case threeInARow of
                    Nothing -> go c pot' xs
                    Just ch -> let (done, rest) = S.partition hasFive pot'
                               in go (c >< fmap fst done) (rest |> (x, ch)) xs
              where hashed = (!! n) . tail . iterate hash $ seed <> (B.pack $ show x)
                    threeInARow = listToMaybe $ findAll find3 hashed
                    pot' = S.dropWhileL ((>1000) . (x-) . fst) pot
                    hasFive p = B.replicate 5 (snd p) `B.isInfixOf` hashed

end :: Seq a -> a
end = (\(_ :> r) -> r) . S.viewr

part1 :: String -> Int
part1 s = end $ find (B.pack s) hash 64 0

part2 :: String -> Int
part2 s = end $ find (B.pack s) hash 64 2016
