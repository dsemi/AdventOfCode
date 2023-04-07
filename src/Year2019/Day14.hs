{-# LANGUAGE NoFieldSelectors, NumDecimals, OverloadedRecordDot, OverloadedStrings #-}

module Year2019.Day14
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Maybe
import FlatParse.Basic

data Reactions = Reactions { graph :: HashMap ByteString (Int, [(Int, ByteString)])
                           , topo :: [ByteString]
                           }

parseReactions :: ByteString -> Reactions
parseReactions input = Reactions graph (topo [] ["FUEL"] incoming)
    where graph = M.fromList $ map parse $ B.lines input
          parse line = case runParser reaction line of
                         OK res _ -> res
                         _ -> error "unreachable"
          chemical = (,) <$> anyAsciiDecimalInt <* $(char ' ') <*> byteStringOf (some $ satisfy isUpper)
          reaction = do
            ins <- some (chemical <* optional_ $(string ", "))
            (n, out) <- $(string " => ") *> chemical
            pure (out, (n, ins))
          incoming = M.fromListWith (+) $ concatMap (map ((,1) . snd) . snd) $ M.elems graph
          topo sorted [] _ = sorted
          topo sorted (x:xs) cnts = let srcs = fromMaybe [] $ fmap snd $ graph M.!? x
                                        (cnts', xs') = foldr (\s (m, rest) ->
                                                                  let m' = M.adjust pred s m
                                                                  in (m', if m' M.! s == 0
                                                                          then s : rest
                                                                          else rest)) (cnts, xs)
                                                       $ map snd srcs
                                    in topo (x:sorted) xs' cnts'

numOre :: Reactions -> Int -> Int
numOre reactions fuel = foldr go (M.singleton "FUEL" fuel) reactions.topo M.! "ORE"
    where go e cnts = case reactions.graph M.!? e of
                        Nothing -> cnts
                        Just (amt, srcs) -> let k = (cnts M.! e + amt - 1) `div` amt
                                            in foldr (\(n, m) -> M.insertWith (+) m (k * n)) cnts srcs


part1 :: ByteString -> Int
part1 = flip numOre 1 . parseReactions

search :: Int -> Int -> (Int -> Int) -> Int
search a b f
    | a == b = if f a > 1e12 then a - 1 else a
    | f mid > 1e12 = search a (mid - 1) f
    | otherwise = search (mid + 1) b f
    where mid = (a + b) `div` 2

part2 :: ByteString -> Int
part2 = search 0 1e12 . numOre . parseReactions
