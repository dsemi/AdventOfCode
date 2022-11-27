module Year2021.Day20
    ( part1
    , part2
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.Char
import Data.List.Split (splitOn)
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed.Mutable (STVector)
import qualified Data.Vector.Unboxed.Mutable as MVU
import Data.Word

advance :: STVector s Word8 -> Vector Word8 -> Word16 -> Word8 -> Int -> ST s ()
advance grid quads xorIn xorOut dim = do
  prev <- MVU.replicate 100 0
  let go r quad c = do
        let p = 100*r + c
        pr <- MVU.read prev c
        gr <- MVU.read grid p
        let quad' = ((quad .&. 0x3333) `shiftL` 2) .|. ((fromIntegral pr `shiftL` 8) .|. fromIntegral gr)
        MVU.write prev c gr
        MVU.write grid p $ (quads ! fromIntegral (quad' `xor` xorIn)) `xor` fromIntegral xorOut
        pure quad'
  forM_ [0 .. dim - 1] $ \r ->
      foldM_ (go r) 0 [0 .. dim - 1]

run :: Int -> String -> Int
run times input = runST $ do
  let [iea, imgL] = splitOn "\n\n" input
      img = VU.fromList $ map (fromIntegral . ord) imgL
  rls <- MVU.replicate 2048 0
  forM_ (zip (iterate ((.&. 0x777) . (+ (0x88+1))) 0) iea) $ \(idx, c) ->
      MVU.write rls idx $ fromIntegral (ord c) .&. 1
  rules <- VU.freeze rls
  let quads = VU.fromList [ ((rules ! ((i `shiftR` 5) .&. 0x777)) `shiftL` 5)
                          .|. ((rules ! ((i `shiftR` 4) .&. 0x777)) `shiftL` 4)
                          .|. ((rules ! ((i `shiftR` 1) .&. 0x777)) `shiftL` 1)
                          .|. ((rules ! ((i `shiftR` 0) .&. 0x777)) `shiftL` 0)
                          | i <- [0..65535] ]
  grid <- MVU.replicate 10000 0
  let outer idx r = (+102) <$> foldM (inner r) idx [0..49]
      inner r idx c = do
        let quad = (((img ! (idx + 0)) .&. 1) `shiftL` 5)
                   .|. (((img ! (idx + 1)) .&. 1) `shiftL` 4)
                   .|. (((img ! (idx + 101)) .&. 1) `shiftL` 1)
                   .|. (((img ! (idx + 102)) .&. 1) `shiftL` 0)
        MVU.write grid (100*r + c) quad
        pure $ idx + 2
  foldM_ outer 0 [0..49]
  let xorIn = if quads ! 0 /= 0 then 0xffff else 0
      xorOut = if quads ! 0 /= 0 then 0x33 else 0
  forM_ [0, 2 .. times-1] $ \step -> do
    advance grid quads 0 xorOut (50 + step + 1)
    advance grid quads xorIn 0 (50 + step + 2)
  MVU.foldl' (\acc v -> acc + popCount v) 0 grid

part1 :: String -> Int
part1 = run 2

part2 :: String -> Int
part2 = run 50
