module Year2022.Day16
    ( part1
    , part2
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Array.Base
import Data.Array.ST
import Data.Bits ((.&.), (.|.), shiftL)
import qualified Data.HashMap.Strict as M
import Data.List (find, foldl', sortBy, tails)
import Data.List.Split
import Data.Ord
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word

solve :: Word16 -> Int -> String -> [Word16]
solve time bins input = V.toList $ runST $ do
                          v <- MV.replicate bins 0
                          dfs2 v (ui M.! "AA") 0 0 time
                          V.unsafeFreeze v
    where valves = lines input
          sz = length valves - 1
          ui = M.fromList $ zip (map ((!! 1) . words) valves) [0..]
          flowRates = V.fromList $ map (read . (!! 1) . splitOneOf "=;") valves
          workingValves = sortBy (comparing (negate . (flowRates V.!))) $ V.toList
                          $ V.map fst $ V.filter ((> 0) . snd) $ V.imap (,) flowRates
          tunnels = map (drop 4 . split (condense $ dropDelims $ oneOf ", ") . (!! 1) . splitOn "; ") valves
          dist :: UArray (Int, Int) Word16
          dist = runSTUArray $ do
            arr <- newArray ((0, 0), (sz, sz)) maxBound
            forM_ (zip [0..] $ map (map (ui M.!)) tunnels) $ \(i, js) -> do
              writeArray arr (i, i) 0
              forM_ js $ \j -> writeArray arr (i, j) 1
            forM_ [0..sz] $ \k ->
                forM_ [0..sz] $ \i ->
                    forM_ [0..sz] $ \j -> do
                      c <- toInteger <$> readArray arr (i, j)
                      a <- toInteger <$> readArray arr (i, k)
                      b <- toInteger <$> readArray arr (k, j)
                      writeArray arr (i, j) $ fromInteger $ min c (a + b)
            pure arr
          upperBd :: Word16 -> Word16 -> Word16 -> Word16
          upperBd openValves pressure timeLeft =
              pressure +
              sum (zipWith (*) [ timeLeft - 2, timeLeft - 4 .. 0]
                               [ flowRates V.! j | j <- workingValves, openValves .&. (1 `shiftL` j) == 0 ])
          dfs2 v i openValves pressure timeLeft = do
            let upper = upperBd openValves pressure timeLeft
            let idx = fromIntegral openValves `mod` MV.length v
            e <- MV.read v idx
            unless (upper <= e) $ do
              when (pressure > e) $ MV.write v idx pressure
              forM_ (zip [0..] workingValves) $ \(bit, j) -> do
                let b = 1 `shiftL` bit
                let d = dist ! (i, j)
                when (b .&. openValves == 0 && d < timeLeft - 1) $ do
                  let timeLeft' = timeLeft - d - 1
                  dfs2 v j (openValves .|. b) (pressure + timeLeft' * flowRates V.! j) timeLeft'

part1 :: String -> Word16
part1 = maximum . solve 30 1

part2 :: String -> Word16
part2 input = foldl' (\best ((hOpens, hPressure) : rest) -> maybe best snd
                                                            $ find ((== 0) . (hOpens .&.) . fst)
                                                            $ takeWhile ((>best) . snd)
                                                            $ map (\(o, ePressure) -> (o, hPressure + ePressure))
                                                            $ rest) 0 $ init $ tails bestPressures
    where bestPressures = sortBy (comparing (negate . snd)) $ filter ((> 0) . snd)
                          $ zip [(0::Word16)..] $ solve 26 (fromIntegral (maxBound :: Word16)) input
