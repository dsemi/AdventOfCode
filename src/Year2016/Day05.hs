module Year2016.Day05
    ( part1
    , part2
    ) where

import Control.Monad (replicateM)
import Control.Monad.Trans.State (State, evalState, state)
import Crypto.Hash.MD5 (hash)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Base16 (encode)
import Data.Monoid ((<>))
import qualified Data.HashMap.Strict as M


findHashPrefixedByFiveZeros :: ByteString -> State Int ByteString
findHashPrefixedByFiveZeros seed =
    state $ \start -> head [ (checksum, num + 1) | num <- [start..]
                           , let checksum = encode . hash . (seed <>) . B.pack $ show num
                           , fiveZeros `B.isPrefixOf` checksum]
        where fiveZeros = B.pack "00000"

part1 :: String -> String
part1 s = B.unpack $ evalState (B.pack . map (B.head . B.drop 5) <$> replicateM 8 nextHash) 0
    where nextHash = findHashPrefixedByFiveZeros $ B.pack s

part2 :: String -> String
part2 s = B.unpack $ evalState (findPassword M.empty) 0
    where nextHash = findHashPrefixedByFiveZeros $ B.pack s
          indices = "01234567"
          addChar m pos char
              | pos `elem` indices && not (M.member pos m) = M.insert pos char m
              | otherwise                                  = m
          findPassword m
              | M.size m == 8 = return . B.pack $ map (m M.!) indices
              | otherwise = do
                  (p : c : []) <- B.unpack . B.take 2 . B.drop 5 <$> nextHash
                  findPassword $ addChar m p c
