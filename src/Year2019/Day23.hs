module Year2019.Day23
    ( part1
    , part2
    ) where

import Control.Monad.Free
import Data.Either
import Data.List (find)
import qualified Data.IntMap as M

import Year2019.IntCode


data Packet = Packet { _addr :: Int
                     , _x :: Int
                     , _y :: Int
                     }

sendInputs :: [Int] -> Free Action r -> Free Action r
sendInputs [] a = a
sendInputs (x:xs) (Free (Input f)) = sendInputs xs $ f x
sendInputs xs (Free (Output o f)) = Free (Output o (sendInputs xs f))
sendInputs _ _ = error "Sent input to terminated program"

getPacket :: Free Action r -> ([Packet], Free Action r)
getPacket (Free (Output o1 (Free (Output o2 (Free (Output o3 k)))))) = ([Packet o1 o2 o3], k)
getPacket k = ([], k)

isInput :: Free Action r -> Bool
isInput (Free (Input _)) = True
isInput _ = False

network :: Memory -> [Either Int Int]
network mem = go 0 0 computers
    where computers = M.fromList [ (i, sendInputs [i] $ runPure mem) | i <- [0..49] ]
          go x y comps =
              case traverse getPacket comps of
                ([], comps')
                    | all isInput next -> Right y : go x y (M.adjust (sendInputs [x,y]) 0 comps')
                    | otherwise -> go x y next
                    where next = fmap (sendInputs [-1]) comps
                (packets, comps') -> send x y packets comps'
          send nx ny [] = go nx ny
          send _ _   (Packet 255 x y : packets) = (Left y :) . send x y packets
          send nx ny (Packet a   x y : packets) = send nx ny packets . M.adjust (sendInputs [x,y]) a

part1 :: String -> Maybe (Either Int Int)
part1 = find isLeft . network . parse

part2 :: String -> (Either Int Int)
part2 = firstDup . filter isRight . network . parse
    where firstDup xs = fst $ head $ filter (uncurry (==)) $ zip xs $ tail xs
