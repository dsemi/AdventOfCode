{-# LANGUAGE NamedFieldPuns, TemplateHaskell, ViewPatterns #-}

module Year2017.Day18
    ( part1
    , part2
    ) where

import Data.Array
import Conduit
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Lens
import Data.Either.Utils (maybeToEither)
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
                       , recv :: Int -> m (Maybe Int)
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
step (Socket {send, recv}) sim =
    maybe (pure Nothing) (runMaybeT . eval) $ sim ^? (instrs . ix (sim ^. line))
    where value = either (\v -> sim ^?! (regs . ix v)) id
          eval (Snd v) = do
            lift $ send (value v)
            pure $ sim & line +~ 1
          eval (Set r v) = pure $ sim & line +~ 1 & (regs . ix r) .~ value v
          eval (Add r v) = pure $ sim & line +~ 1 & (regs . ix r) %~ (+value v)
          eval (Mul r v) = pure $ sim & line +~ 1 & (regs . ix r) %~ (*value v)
          eval (Mod r v) = pure $ sim & line +~ 1 & (regs . ix r) %~ (`mod` value v)
          eval (Rcv r) = do
            val <- lift $ recv $ value $ Left r
            case val of
              Just v -> pure $ sim & line +~ 1 & (regs . ix r) .~ v
              Nothing -> MaybeT $ pure Nothing
          eval (Jgz a b) = pure $ sim & line +~ (if value a > 0 then value b else 1)

run :: (Monad m) => Socket m -> Sim -> m ()
run socket = go . Just
    where go Nothing = pure ()
          go (Just sim) = step socket sim >>= go

part1 :: String -> Int
part1 input = let sent = runIdentity $ sourceToList $
                         yieldMany sent .| run socket (parseInstrs input)
              in last sent
    where socket = Socket { send = yield
                          , recv = \v -> if v /= 0
                                         then pure Nothing -- Stop execution
                                         else await
                          }

data Action = Send Int | Receive

isSend :: Action -> Bool
isSend (Send _) = True
isSend _ = False

part2 :: String -> Int
part2 input =
    let sim0 = parseInstrs input
        sim1 = (regs . ix 'p') .~ 1 $ parseInstrs input
        recvCount = 0 :: Int
        socket = Socket { send = \val -> modify' succ >> yield (Send val)
                        , recv = const $ do
                            yield Receive
                            let go = do
                                  val <- await
                                  n <- get
                                  case val of
                                    Just Receive ->
                                        if n == 0
                                        then pure Nothing
                                        else modify' pred >> go
                                    Just (Send v) -> pure $ Just v
                                    Nothing -> pure Nothing
                            go
                        }
        sent0 = flip evalState recvCount $ sourceToList $ yieldMany sent1 .| run socket sim0
        sent1 = flip evalState recvCount $ sourceToList $ yieldMany sent0 .| run socket sim1
    in length $ filter isSend sent1
