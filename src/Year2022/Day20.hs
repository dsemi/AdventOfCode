module Year2022.Day20
    ( part1
    , part2
    ) where

import Control.Monad
import Control.Monad.ST
import Data.List (elemIndex)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

mix :: Int -> Int -> String -> Int
mix scale times input = runST $ do
  locs <- V.thaw locations
  forM_ [1..times] $ \_ ->
      forM_ (zip [0..] ns) $ \(i, n) -> do
        Just loc <- V.elemIndex i <$> V.unsafeFreeze locs
        MV.move (MV.slice loc (m - loc - 1) locs) (MV.slice (loc + 1) (m - loc - 1) locs)
        let idx = (n + loc) `mod` (m - 1)
        MV.move (MV.slice (idx + 1) (m - idx - 1) locs) (MV.slice idx (m - idx - 1) locs)
        MV.write locs idx i
  v <- V.freeze locs
  let Just z = (`V.elemIndex` v) =<< elemIndex 0 ns
  pure $ ns !! (v V.! ((z + 1000) `mod` m)) +
       ns !! (v V.! ((z + 2000) `mod` m)) +
       ns !! (v V.! ((z + 3000) `mod` m))
    where ns = map ((*scale) . read) $ lines input
          m = length ns
          locations = V.fromList $ [0 .. m - 1]

part1 :: String -> Int
part1 = mix 1 1

part2 :: String -> Int
part2 = mix 811589153 10
