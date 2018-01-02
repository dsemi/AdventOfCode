{-# LANGUAGE RecordWildCards, TemplateHaskell, ViewPatterns #-}

module Year2017.Day18
    ( part1
    , part2
    ) where

import Data.Array
import Control.Concurrent (forkIO, killThread, newChan, readChan, writeChan)
import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Lens
import Data.Conduit
import Data.Either.Utils (maybeToEither)
import Data.IORef
import Data.Maybe
import Data.String.Utils (maybeRead)
import Data.Vector (Vector, fromList)


type Register = Char
type Value = Either Register Int
data Instr = Snd Value
           | Set Register Value
           | Add Register Value
           | Mul Register Value
           | Mod Register Value
           | Rcv Register
           | Jgz Value Value

data Sim = Sim { _regs :: Array Register Int
               , _line :: Int
               , _instrs :: Vector Instr
               }
makeLenses ''Sim

data Socket m = Socket { send :: Int -> m ()
                       , recv :: Int -> m Int
                       }

parseInstrs :: String -> Sim
parseInstrs = Sim (listArray ('a', 'z') $ repeat 0) 0 . fromList . map parseInstr . lines
    where parseInstr instr =
              case words instr of
                ["snd",             parse -> v] -> Snd v
                ["set",  head -> r, parse -> v] -> Set r v
                ["add",  head -> r, parse -> v] -> Add r v
                ["mul",  head -> r, parse -> v] -> Mul r v
                ["mod",  head -> r, parse -> v] -> Mod r v
                ["rcv",  head -> r            ] -> Rcv r
                ["jgz", parse -> a, parse -> b] -> Jgz a b
                _ -> error "Parse error"
          parse :: String -> Value
          parse x = maybeToEither (head x) $ maybeRead x

step :: (Monad m) => Socket m -> Sim -> m (Maybe Sim)
step (Socket {..}) sim = traverse eval $ sim ^? (instrs . ix (sim ^. line))
    where value = either (\v -> sim ^?! (regs . ix v)) id
          eval (Snd v) = do
            send (value v)
            pure $ sim & line +~ 1
          eval (Set r v) = pure $ sim & line +~ 1 & (regs . ix r) .~ value v
          eval (Add r v) = pure $ sim & line +~ 1 & (regs . ix r) %~ (+value v)
          eval (Mul r v) = pure $ sim & line +~ 1 & (regs . ix r) %~ (*value v)
          eval (Mod r v) = pure $ sim & line +~ 1 & (regs . ix r) %~ (`mod` value v)
          eval (Rcv r) = do
            v <- recv $ value $ Left r
            pure $ sim & line +~ 1 & (regs . ix r) .~ v
          eval (Jgz a b) = pure $ sim & line +~ (if value a > 0 then value b else 1)

run :: (Monad m) => Socket m -> Sim -> m ()
run socket = go . Just
    where go Nothing = pure ()
          go (Just sim) = step socket sim >>= go

part1 :: String -> Int
part1 input = fromJust $ evalState (run socket (parseInstrs input) $$ await) 0
    where socket = Socket { send = put
                          , recv = \v -> do
                                     val <- get
                                     when (v /= 0) $ yield val
                                     pure val
                          }

part2 :: String -> IO Int
part2 input = do
  counter <- newIORef 0
  chan0 <- newChan
  chan1 <- newChan
  let sim0 = parseInstrs input
      sim1 = (regs . ix 'p') .~ 1 $ parseInstrs input
      socket0 = Socket { send = writeChan chan1
                       , recv = const $ readChan chan0
                       }
      socket1 = Socket { send = \val -> modifyIORef' counter (+1) >> writeChan chan0 val
                       , recv = const $ readChan chan1
                       }
  handle (\BlockedIndefinitelyOnMVar -> return ()) . bracket (forkIO $ run socket0 sim0) killThread
             . const $ run socket1 sim1
  readIORef counter
