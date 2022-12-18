module Year2022.Day16
    ( part1
    , part2
    ) where

import Control.Monad
import Data.Array.Base
import Data.Array.ST
import Data.Bits
import qualified Data.HashMap.Strict as M
import Data.List (tails)
import Data.List.Split
import qualified Data.Vector.Unboxed as V
import Data.Word

solve :: Int -> String -> [(Word64, Int)]
solve time input = dfs (ui M.! "AA") 0 0 time
    where valves = lines input
          sz = length valves - 1
          ui = M.fromList $ zip (map ((!! 1) . words) valves) [0..]
          flowRates = V.fromList $ map (read . (!! 1) . splitOneOf "=;") valves
          workingValves = V.toList $ V.map fst $ V.filter ((/= 0) . snd) $ V.imap (,) flowRates
          tunnels = map (drop 4 . split (condense $ dropDelims $ oneOf ", ") . (!! 1) . splitOn "; ") valves
          dist :: UArray (Int, Int) Int
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
          dfs :: Int -> Word64 -> Int -> Int -> [(Word64, Int)]
          dfs i openValves pressure timeLeft =
              (openValves, pressure)
              : [ v | j <- workingValves
                , let b = 1 `shiftL` j
                , let d = dist ! (i, j)
                , b .&. openValves == 0 && d < timeLeft - 1
                , let timeLeft' = timeLeft - d - 1
                , v <- dfs j (openValves .|. b) (pressure + timeLeft' * flowRates V.! j) timeLeft' ]

part1 :: String -> Int
part1 = maximum . map snd . solve 30

part2 :: String -> Int
part2 input = maximum [ hPressure + ePressure | bps <- init $ tails bestPressures 
                      , let (hOpens, hPressure) = head bps
                      , (eOpens, ePressure) <- tail bps
                      , hOpens .&. eOpens == 0 ]
    where bestPressures = M.toList $ M.fromListWith max $ solve 26 input
