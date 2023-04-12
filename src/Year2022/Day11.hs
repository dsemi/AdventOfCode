{-# LANGUAGE NoFieldSelectors, OverloadedRecordDot#-}

module Year2022.Day11
    ( part1
    , part2
    ) where

import Control.Monad
import Control.Monad.ST
import Data.ByteString (ByteString)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as MVU
import qualified Data.Vector.Algorithms.Intro as VA
import FlatParse.Basic

data Monkey = Monkey { _num :: Int
                     , items :: [Int]
                     , op :: Int -> Int
                     , divisor :: Int
                     , true :: Int
                     , false :: Int
                     }

monkeys :: ByteString -> [Monkey]
monkeys input = case runParser (some $ monkey <* optional_ $(string "\n\n")) input of
                  OK res _ -> res
                  _ -> error "unreachable"
    where monkey = do
            n <- $(string "Monkey ") *> anyAsciiDecimalInt <* $(string ":\n")
            items <- $(string "  Starting items: ") *> some (anyAsciiDecimalInt <* optional_ $(string ", ")) <*
                     $(char '\n')
            (c, arg) <- (,) <$> ($(string "  Operation: new = old ") *>
                                 (($(char '+') >> pure (+)) <|> ($(char '*') >> pure (*))) <* $(char ' '))
                         <*> ((Left <$> $(string "old")) <|> (Right <$> anyAsciiDecimalInt)) <* $(char '\n')
            test <- $(string "  Test: divisible by ") *> anyAsciiDecimalInt <* $(char '\n')
            true <- $(string "    If true: throw to monkey ") *> anyAsciiDecimalInt <* $(char '\n')
            false <- $(string "    If false: throw to monkey ") *> anyAsciiDecimalInt
            let op = case arg of
                       Left _ -> (^2)
                       Right a -> c a
            pure $ Monkey n items op test true false

solve :: Bool -> ByteString -> Int
solve p2 input = runST $ do
  v <- V.thaw $ V.fromList monks
  MVU.replicate (length monks) 0 >>= go iters v
    where monks = monkeys input
          mx = length monks - 1
          m = foldr1 lcm $ map (.divisor) monks
          iters = if p2 then 10000 else 20
          go 0 _ ins = do
            VA.sort ins
            (*) <$> MVU.read ins (mx - 1) <*> MVU.read ins mx
          go c mks ins = do
            forM_ [0 .. mx] $ \i -> do
              mk <- MV.read mks i
              MVU.modify ins (+ length (mk.items)) i
              forM_ (mk.items) $ \it -> do
                let worryLevel = if p2 then mk.op it `mod` m else mk.op it `div` 3
                if worryLevel `mod` mk.divisor == 0
                then MV.modify mks (\x -> x { items = x.items ++ [worryLevel]}) $ mk.true
                else MV.modify mks (\x -> x { items = x.items ++ [worryLevel]}) $ mk.false
              MV.modify mks (\x -> x { items = [] }) i
            go (c-1) mks ins

part1 :: ByteString -> Int
part1 = solve False

part2 :: ByteString -> Int
part2 = solve True
