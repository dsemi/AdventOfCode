{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}

module Year2017.Day18
    ( part1
    , part2
    ) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.Array.Unboxed
import Data.Either
import Data.Maybe
import Data.Vector (Vector, fromList)
import Text.Megaparsec
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
type Recv m = Int -> m Int

parseInstrs :: String -> Sim
parseInstrs = Sim (listArray ('a', 'z') $ repeat 0) 0 . fromList
              . map (fromJust . parseMaybe instr) . lines
    where instr :: Parsec () String Instr
          instr = choice [ string "snd " >> Snd <$> value
                         , string "set " >> Set <$> letterChar <* spaceChar <*> value
                         , string "add " >> Add <$> letterChar <* spaceChar <*> value
                         , string "mul " >> Mul <$> letterChar <* spaceChar <*> value
                         , string "mod " >> Mod <$> letterChar <* spaceChar <*> value
                         , string "rcv " >> Rcv <$> letterChar
                         , string "jgz " >> Jgz <$> value <* spaceChar <*> value
                         ]
          value = eitherP letterChar int
          int = signed (pure ()) decimal

reg :: Applicative f => Char -> (Int -> f Int) -> Sim -> f Sim
reg r = regs . ix r

step :: (Monad m) => Send m -> Recv m -> Sim -> m (Maybe Sim)
step send recv sim = traverse eval $ sim ^? (instrs . ix (sim ^. line))
    where value = either (\v -> sim ^?! (regs . ix v)) id
          eval instr = do
            f <- case instr of
                   (Snd v) -> send (value v) >> pure id
                   (Set r v) -> pure $ reg r .~ value v
                   (Add r v) -> pure $ reg r +~ value v
                   (Mul r v) -> pure $ reg r *~ value v
                   (Mod r v) -> pure $ reg r %~ (`mod` value v)
                   (Rcv r) -> (reg r .~) <$> recv (value (Left r))
                   (Jgz a b) -> pure $ if value a > 0 then line +~ value b - 1 else id
            pure $ sim & f & line +~ 1

run :: (Monad m) => Send m -> Recv m -> Sim -> m ()
run send recv sim = go
    where go = step send recv sim >>= maybe (pure ()) (run send recv)

part1 :: String -> Int
part1 input = fromLeft 0 . runExcept $ evalStateT (run send recv (parseInstrs input)) 0
    where send = put
          recv v = when (v /= 0) (get >>= throwError) >> pure 0

part2 :: String -> Int
part2 input =
    let sim0 = parseInstrs input
        sim1 = sim0 & reg 'p' .~ 1
        send v = modify' (_1 %~ (+1)) >> tell [Just v]
        recv _ = tell [Nothing] >> get >>= go
            where go (n, Just v : rest) = put (n, rest) >> pure v
                  go (n, Nothing : rest) | n > 0 = go (n-1, rest)
                  go _ = throwError ()
        runSim sim prog = flip evalState (0 :: Int, prog) . execWriterT . runExceptT
                          . flip catchError (const $ pure ()) $ run send recv sim
        p0 = runSim sim0 p1
        p1 = runSim sim1 p0
    in length $ catMaybes p1
