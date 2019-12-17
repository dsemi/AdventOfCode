{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module Year2017.Day23
    ( part1
    , part2
    ) where

import Data.Array
import Control.Lens
import Control.Monad.State
import Data.Either.Utils
import Data.String.Utils
import Data.Vector (Vector)
import qualified Data.Vector as V
import Math.NumberTheory.Primes.Testing


type Register = Char
type Value = Either Register Int
data Instr = Set Register Value
           | Sub Register Value
           | Mul Register Value
           | Jnz Value Value deriving (Show)

data Sim = Sim { _regs :: Array Register Int
               , _line :: Int
               , _instrs :: Vector Instr
               } deriving (Show)
makeLenses ''Sim

data Tracker m = Tracker { mul :: m ()
                         }

parseInstrs :: String -> Sim
parseInstrs = Sim (listArray ('a', 'h') $ repeat 0) 0 . V.fromList . map parseLine . lines
    where parseLine ln =
              case words ln of
                ["set",  head -> r, parse -> v] -> Set r v
                ["sub",  head -> r, parse -> v] -> Sub r v
                ["mul",  head -> r, parse -> v] -> Mul r v
                ["jnz", parse -> a, parse -> b] -> Jnz a b
                _ -> error "Invalid instruction"
          parse :: String -> Value
          parse x = maybeToEither (head x) $ maybeRead x

step :: (Monad m) => Tracker m -> Sim -> m (Maybe Sim)
step tracker sim = traverse eval $ sim ^? (instrs . ix (sim ^. line))
    where value :: Value -> Int
          value = either (\v -> sim ^?! (regs . ix v)) id
          eval (Set r v) = pure $ line +~ 1 $ (regs . ix r) .~ value v $ sim
          eval (Sub r v) = pure $ line +~ 1 $ (regs . ix r) %~ (subtract $ value v) $ sim
          eval (Mul r v) = do
            mul tracker
            pure $ line +~ 1 $ (regs . ix r) %~ (*value v) $ sim
          eval (Jnz a b) = pure $ line +~ (if value a /= 0 then value b else 1) $ sim

run :: (Monad m) => Tracker m -> (Tracker m -> Sim -> m (Maybe Sim)) -> Sim -> m Sim
run tracker next = go
    where go sim = do
            sim' <- next tracker sim
            case sim' of
              Just s -> go s
              Nothing -> pure sim

part1 :: String -> Int
part1 = flip execState 0 . run tracker step . parseInstrs
    where tracker = Tracker { mul = modify' succ }

data PrimalityCheck = PCheck { toCheck :: Register
                             , innerCounter :: Register
                             , outerCounter :: Register
                             , workspace :: Register
                             , primeCheck :: Register
                             }

primalityCheck :: Vector Instr -> Maybe PrimalityCheck
primalityCheck ins = do
  guard $ V.length ins >= 14
  Set                     e             (Right 2) <- pure $ V.unsafeIndex ins 0
  Set                     g              (Left d) <- pure $ V.unsafeIndex ins 1
  Mul      ((== g) -> True) ((== Left e) -> True) <- pure $ V.unsafeIndex ins 2
  Sub      ((== g) -> True)              (Left b) <- pure $ V.unsafeIndex ins 3
  Jnz ((== Left g) -> True)             (Right 2) <- pure $ V.unsafeIndex ins 4
  Set                     f             (Right 0) <- pure $ V.unsafeIndex ins 5
  Sub      ((== e) -> True)          (Right (-1)) <- pure $ V.unsafeIndex ins 6
  Set      ((== g) -> True) ((== Left e) -> True) <- pure $ V.unsafeIndex ins 7
  Sub      ((== g) -> True) ((== Left b) -> True) <- pure $ V.unsafeIndex ins 8
  Jnz ((== Left g) -> True)          (Right (-8)) <- pure $ V.unsafeIndex ins 9
  Sub      ((== d) -> True)          (Right (-1)) <- pure $ V.unsafeIndex ins 10
  Set      ((== g) -> True) ((== Left d) -> True) <- pure $ V.unsafeIndex ins 11
  Sub      ((== g) -> True) ((== Left b) -> True) <- pure $ V.unsafeIndex ins 12
  Jnz ((== Left g) -> True)         (Right (-13)) <- pure $ V.unsafeIndex ins 13
  pure $ PCheck b e d g f

step' :: (Monad m) => Tracker m -> Sim -> m (Maybe Sim)
step' tracker sim =
    case (primalityCheck $ V.drop (sim ^. line) $ sim ^. instrs) of
      Just (PCheck {toCheck, innerCounter, outerCounter, workspace, primeCheck}) ->
          let b = sim ^?! (regs . ix toCheck)
              sim' = line +~ 14
                     $ (regs . ix primeCheck) .~ (if isPrime (fromIntegral b) then 1 else 0)
                     $ (regs . ix workspace) .~ 0
                     $ (regs . ix outerCounter) .~ b
                     $ (regs . ix innerCounter) .~ b
                     $ sim
          in pure $ Just sim'
      Nothing -> step tracker sim

part2 :: String -> Int
part2 = (^?! (regs . ix 'h')) . runIdentity
        . run tracker step' . ((regs . ix 'a') .~ 1) . parseInstrs
    where tracker = Tracker { mul = pure () }
