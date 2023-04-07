{-# LANGUAGE BangPatterns, FlexibleContexts, RankNTypes #-}

module Year2021.Day24
    ( part1
    , part2
    ) where

import Control.Applicative (asum)
import Control.Lens
import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Bool
import qualified Data.IntSet as S
import Data.Maybe
import FlatParse.Basic

import Utils

type Value = Either Char Int

data Instr = Inp Char
           | Add Char Value
           | Mul Char Value
           | Div Char Value
           | Mod Char Value
           | Eql Char Value
             deriving (Show)

data Prog = Prog { _w :: Int
                 , _x :: Int
                 , _y :: Int
                 , _z :: Int
                 , _instrs :: [Instr]
                 } deriving (Show)
makeLenses ''Prog

reg :: Functor f => Char -> (Int -> f Int) -> Prog -> f Prog
reg 'w' = w
reg 'x' = x
reg 'y' = y
reg 'z' = z
reg _ = error "Invalid register"

parseInstrs :: ByteString -> Prog
parseInstrs = Prog 0 0 0 0 . map parse . B.lines
    where register = satisfy (`elem` "wxyz")
          value = (Left <$> register) <|> (Right <$> signedInt)
          instr = asum [ $(string "inp ") >> Inp <$> register
                       , $(string "add ") >> Add <$> register <* $(char ' ') <*> value
                       , $(string "mul ") >> Mul <$> register <* $(char ' ') <*> value
                       , $(string "div ") >> Div <$> register <* $(char ' ') <*> value
                       , $(string "mod ") >> Mod <$> register <* $(char ' ') <*> value
                       , $(string "eql ") >> Eql <$> register <* $(char ' ') <*> value
                       ]
          parse line = case runParser instr line of
                         OK res _ -> res
                         _ -> error "unreachable"

val :: Prog -> Value -> Int
val sim = either ((sim ^.) . reg) id

runNext :: Prog -> Int -> Maybe (Int, Prog)
runNext prog n = go $ eval 0 prog
    where go (a, p) = case p ^. instrs of
                        [] -> final
                        (Inp _ : _) -> final
                        _ -> go $ eval a p
              where final = if a /= 26 || p ^. x == 0 then Just (n, p) else Nothing
          eval a p = over _2 (over instrs tail p &) $
                     case head (p ^. instrs) of
                       Inp r -> (a, reg r .~ n)
                       Add r v -> (a, reg r +~ value v)
                       Mul r v -> (a, reg r *~ value v)
                       Div 'z' (Right v) -> (v, z %~ (`div` v))
                       Div r v -> (a, reg r %~ (`div` value v))
                       Mod r v -> (a, reg r %~ (`mod` value v))
                       Eql r v -> (a, reg r %~ (bool 0 1 . (==value v)))
              where value = val p

dfs :: Bool -> Prog -> Maybe Int
dfs p2 program = evalState (go 0 14 program) S.empty
    where go !n !d prog
              | d == 0 = pure $ if prog ^. z == 0 then Just n else Nothing
              | otherwise = get >>= \vis -> if S.member key vis then pure Nothing else recur
              where recur = do
                      let ps = mapMaybe (runNext prog) ds
                      v <- foldM (\acc (i, p) -> if isJust acc then pure acc
                                                 else go (n*10 + i) (d-1) p) Nothing ps
                      when (isNothing v) $ modify' $ S.insert key
                      pure v
                    key = prog ^. z * 100 + d
          ds = if p2 then [1..9] else [9, 8..1]

part1 :: ByteString -> Maybe Int
part1 = dfs False . parseInstrs

part2 :: ByteString -> Maybe Int
part2 = dfs True . parseInstrs
