{-# LANGUAGE FlexibleContexts #-}

module Year2021.Day16
    ( part1
    , part2
    ) where

import Control.Monad.Loops
import Control.Monad.State.Strict
import Data.Char
import Data.List (foldl1')
import Data.Tuple
import Text.Printf


packet :: (MonadState String m) => m (Int, Int)
packet = do
  v <- bin 3
  typeId <- bin 3
  if typeId == 4
  then (v,) <$> parseId
  else do
    (vs, ns) <- fmap unzip $ bin 1 >>= \x -> if x == 0 then parseLTid0 else parseLTid1
    case typeId of
      0 -> pure (v + sum vs, sum ns)
      1 -> pure (v + sum vs, product ns)
      2 -> pure (v + sum vs, minimum ns)
      3 -> pure (v + sum vs, maximum ns)
      5 -> pure (v + sum vs, if ns !! 0 > ns !! 1 then 1 else 0)
      6 -> pure (v + sum vs, if ns !! 0 < ns !! 1 then 1 else 0)
      7 -> pure (v + sum vs, if ns !! 0 == ns !! 1 then 1 else 0)
      _ -> error $ "Invalid type id: " ++ show typeId
    where bin n = foldl1' (\a b -> a * 2 + b) . map digitToInt <$> state (splitAt n)
          parseId = f 0
              where f n = bin 5 >>= \x -> (if x >= 16 then f else pure) $ n * 16 + x `mod` 16
          parseLTid0 = do
            rest <- bin 15 >>= \n -> state (swap . splitAt n)
            unfoldM (get >>= \s -> if null s then pure Nothing else Just <$> packet) <* put rest
          parseLTid1 = bin 11 >>= \n -> mapM (const packet) [1..n]

toBin :: String -> String
toBin = concatMap (printf "%04b" . digitToInt)

part1 :: String -> Int
part1 = fst . evalState packet . toBin

part2 :: String -> Int
part2 = snd . evalState packet . toBin
