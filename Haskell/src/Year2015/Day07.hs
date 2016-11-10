module Year2015.Day07
    ( part1
    , part2
    ) where

import Data.Bits
import Data.Either
import Data.Function.Memoize
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Data.List (foldl')
import Data.Either.Utils
import Data.String.Utils

type Id = Either String Int
data Node = Const Id
          | Not Id
          | And Id Id
          | Or Id Id
          | LShift Id Id
          | RShift Id Id

parseNode :: String -> (String, Node)
parseNode line = case words line of
                   [             a, "->", v] -> (v, Const $ f a)
                   [   "NOT",    a, "->", v] -> (v, Not $ f a)
                   [a, "AND",    b, "->", v] -> (v, f a `And` f b)
                   [a, "OR",     b, "->", v] -> (v, f a `Or` f b)
                   [a, "LSHIFT", b, "->", v] -> (v, f a `LShift` f b)
                   [a, "RSHIFT", b, "->", v] -> (v, f a `RShift` f b)
    where f :: String -> Either String Int
          f x = maybeToEither x $ maybeRead x

eval :: (String -> Int) -> Node -> Int
eval f exp = case exp of
               Const a    -> val a
               Not a      -> complement $ val a
               And a b    -> val a .&. val b
               Or a b     -> val a .|. val b
               LShift a b -> val a `shiftL` val b
               RShift a b -> val a `shiftR` val b
    where val = either f id

buildWires :: [String] -> HashMap String Node
buildWires = foldl' addWire M.empty
    where addWire :: HashMap String Node -> String -> HashMap String Node
          addWire m s = let (w, node) = parseNode s
                        in M.insert w node m

getValue :: HashMap String Node -> String -> Int
getValue m = mgv
    where mgv = memoFix gv
          gv f k = eval f $ m ! k

p1 :: String -> Int
p1 input = let m = buildWires $ lines input
           in getValue m "a"

part1 :: String -> String
part1 = show . p1

p2 :: String -> Int
p2 input = let m = buildWires $ lines input
               a = getValue m "a"
               m' = M.insert "b" (Const $ Right a) m
           in getValue m' "a"

part2 :: String -> String
part2 = show . p2
