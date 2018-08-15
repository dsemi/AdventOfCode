{-# LANGUAGE NamedFieldPuns, TemplateHaskell, ViewPatterns #-}

module Year2017.Day18
    ( part1
    , part2
    ) where

import Utils

import Control.Lens
import Control.Monad.Cont
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.ST
import Control.Monad.State
import Data.Array.Unboxed
import Data.Maybe
import Data.STRef
import qualified Data.Sequence as S
import Data.Vector (Vector, fromList)
import Text.Megaparsec (choice, eitherP, parseMaybe)
import Text.Megaparsec.Char (letterChar, spaceChar, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)


type Register = Char
type Value = Either Register Int
data Instr = Snd Value
           | Set Register Value
           | Add Register Value
           | Mul Register Value
           | Mod Register Value
           | Rcv Register
           | Jgz Value Value

data Sim = Sim { _regs :: UArray Register Int
               , _line :: Int
               , _instrs :: Vector Instr
               }
makeLenses ''Sim

type Send m = Int -> m ()
type Recv m = Int -> m (Maybe Int)

parseInstrs :: String -> Sim
parseInstrs = Sim (listArray ('a', 'z') $ repeat 0) 0 . fromList
              . map (fromJust . parseMaybe instr) . lines
    where instr :: Parser Instr
          instr = choice [ string "snd " >> Snd <$> value
                         , string "set " >> Set <$> letterChar <* spaceChar <*> value
                         , string "add " >> Add <$> letterChar <* spaceChar <*> value
                         , string "mul " >> Mul <$> letterChar <* spaceChar <*> value
                         , string "mod " >> Mod <$> letterChar <* spaceChar <*> value
                         , string "rcv " >> Rcv <$> letterChar
                         , string "jgz " >> Jgz <$> value <* spaceChar <*> value
                         ]
          value :: Parser Value
          value = eitherP letterChar int
          int :: Parser Int
          int = signed (pure ()) decimal

reg :: Applicative f => Char -> (Int -> f Int) -> Sim -> f Sim
reg r = regs . ix r

step :: (Monad m) => Send m -> Recv m -> Instr -> Sim -> m Sim
step send recv instr sim = do
  let value = either (\v -> sim ^?! (regs . ix v)) id
  f <- case instr of
         (Snd v) -> send (value v) >> pure id
         (Set r v) -> pure $ reg r .~ value v
         (Add r v) -> pure $ reg r +~ value v
         (Mul r v) -> pure $ reg r *~ value v
         (Mod r v) -> pure $ reg r %~ (`mod` value v)
         (Rcv r) -> maybe id (reg r .~) <$> recv (value (Left r))
         (Jgz a b) -> pure $ if value a > 0 then line +~ value b - 1 else id
  pure $ sim & f & line +~ 1

run :: (Monad m) => Send m -> Recv m -> Sim -> m ()
run send recv sim =
    case sim ^? (instrs . ix (sim ^. line)) of
      Just instr -> step send recv instr sim >>= run send recv
      Nothing -> pure ()

part1 :: String -> Int
part1 input = flip runCont fromJust $ callCC $ \k -> do
                let send = put
                    recv = \v -> do
                      val <- Just <$> get
                      when (v /= 0) $ lift $ k val
                      pure val
                evalStateT (run send recv (parseInstrs input)) 0
                pure Nothing

part2 :: String -> Int
part2 input = runST $ do
  p1Sends <- newSTRef 0
  p0Queue <- newSTRef S.empty
  p1Queue <- newSTRef S.empty
  let sim0 = parseInstrs input
      sim1 = sim0 & reg 'p' .~ 1
      send queue v = lift $ modifySTRef' queue (S.|> v)
      recv queue1Ref queue2Ref _ = request (queue1Ref, queue2Ref)
      p1Send v = do
        lift $ modifySTRef' p1Sends (+1)
        send p0Queue v
      dequeue queueRef = S.viewl <$> lift (readSTRef queueRef) >>=
                           \case
                            S.EmptyL -> pure Nothing
                            x S.:< xs -> do
                              lift $ writeSTRef queueRef xs
                              pure $ Just x
  contRef <- do
    Left (Request _ k) <- resume $ run (send p1Queue) (recv p0Queue p1Queue) sim0
    newSTRef k
  pogoStick (\(Request (queue1, queue2) k) -> do
               dequeue queue1 >>=
                 \case
                  Nothing -> dequeue queue2 >>=
                               \case
                                Nothing -> pure ()
                                x -> lift (swapSTRef contRef k) >>= ($ x)
                  x -> k x)
                $ run p1Send (recv p1Queue p0Queue) sim1
  readSTRef p1Sends
