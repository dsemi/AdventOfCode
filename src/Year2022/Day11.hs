{-# LANGUAGE OverloadedStrings #-}

module Year2022.Day11
    ( part1
    , part2
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Algorithms.Intro as VA
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer

data Monkey = Monkey { _num :: Int
                     , items :: [Int]
                     , op :: Int -> Int
                     , divisor :: Int
                     , true :: Int
                     , false :: Int
                     }

monkeys :: String -> [Monkey]
monkeys = fromJust . parseMaybe @() (monkey `sepBy` "\n\n")
    where monkey = do
            n <- "Monkey " *> decimal <* ":\n"
            items <- "  Starting items: " *> decimal `sepBy` ", " <* "\n"
            (c, arg) <- (,) <$> ("  Operation: new = old " *> (("+" >> pure (+)) <|> ("*" >> pure (*))) <* " ")
                         <*> (eitherP "old" decimal) <* "\n"
            test <- "  Test: divisible by " *> decimal <* "\n"
            true <- "    If true: throw to monkey " *> decimal <* "\n"
            false <- "    If false: throw to monkey " *> decimal
            let op = case arg of
                       Left _ -> (^2)
                       Right a -> c a
            pure $ Monkey n items op test true false

solve :: Bool -> String -> Int
solve p2 input = runST $ do
  v <- V.thaw $ V.fromList monks
  MV.replicate (length monks) 0 >>= go iters v
    where monks = monkeys input
          mx = length monks - 1
          m = foldr1 lcm $ map divisor monks
          iters = if p2 then 10000 else 20
          go 0 _ ins = do
            VA.sort ins
            (*) <$> MV.read ins (mx - 1) <*> MV.read ins mx
          go c mks ins = do
            forM_ [0 .. mx] $ \i -> do
              mk <- MV.read mks i
              MV.modify ins (+ length (items mk)) i
              forM_ (items mk) $ \it -> do
                let worryLevel = if p2 then op mk it `mod` m else op mk it `div` 3
                if worryLevel `mod` divisor mk == 0
                then MV.modify mks (\x -> x { items = items x ++ [worryLevel]}) $ true mk
                else MV.modify mks (\x -> x { items = items x ++ [worryLevel]}) $ false mk
              MV.modify mks (\x -> x { items = [] }) i
            go (c-1) mks ins

part1 :: String -> Int
part1 = solve False

part2 :: String -> Int
part2 = solve True
