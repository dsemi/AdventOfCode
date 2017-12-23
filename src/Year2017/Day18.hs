{-# LANGUAGE RecordWildCards, TemplateHaskell, ViewPatterns #-}

module Year2017.Day18
    ( part1
    , part2
    ) where

import Control.Concurrent (forkIO, killThread, newChan, readChan, writeChan)
import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Lens
import Data.Conduit
import Data.Either.Utils (maybeToEither)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Maybe
import Data.IORef
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

data Sim = Sim { _regs :: HashMap Register Int
               , _line :: Int
               , _instrs :: Vector Instr
               }
makeLenses ''Sim

data Socket m = Socket { send :: Int -> m ()
                       , recv :: Int -> m Int
                       }

parseInstrs :: String -> Sim
parseInstrs = Sim M.empty 0 . fromList . map parseInstr . lines
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
    where value = either (\v -> fromMaybe 0 (sim ^. (regs . at v))) id
          eval (Snd v) = do
            send (value v)
            pure $ line +~ 1 $ sim
          eval (Set r v) = pure $ line +~ 1 $ (regs . at r) ?~ value v $ sim
          eval (Add r v) = pure $ line +~ 1
                           $ (regs . at r) %~ (pure . (+value v) . fromMaybe 0) $ sim
          eval (Mul r v) = pure $ line +~ 1
                           $ (regs . at r) %~ (pure . (*value v) . fromMaybe 0) $ sim
          eval (Mod r v) = pure $ line +~ 1
                           $ (regs . at r) %~ (pure . (`mod` value v) . fromMaybe 0) $ sim
          eval (Rcv r) = do
            v <- recv $ value $ Left r
            pure $ line +~ 1 $ (regs . at r) ?~ v $ sim
          eval (Jgz a b) = pure $ line +~ (if value a > 0 then value b else 1) $ sim

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
      sim1 = (regs . at 'p') ?~ 1 $ parseInstrs input
      socket0 = Socket { send = writeChan chan1
                       , recv = const $ readChan chan0
                       }
      socket1 = Socket { send = \val -> modifyIORef' counter (+1) >> writeChan chan0 val
                       , recv = const $ readChan chan1
                       }
  handle (\BlockedIndefinitelyOnMVar -> return ()) . bracket (forkIO $ run socket0 sim0) killThread
             . const $ run socket1 sim1
  readIORef counter
