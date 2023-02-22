module Year2017.Day24
    ( part1
    , part2
    ) where

import Control.Monad
import Control.Monad.ST
import Data.List (foldl')
import Data.List.Split
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU

type Pipe = (Int, Int)

parsePipes :: String -> [Pipe]
parsePipes = map parse . lines
    where parse = f . splitOn "/"
          f [x, y] = (read x, read y)
          f _ = error "Invalid pipe"

solve :: (Ord a) => (a -> Pipe -> a) -> a -> String -> a
solve f start input = go 0 start $ VU.replicate (VU.length pipes) False
    where pipes = VU.fromList $ parsePipes input
          mx = succ $ VU.maximum $ VU.map (uncurry max) pipes
          (as, bs) = runST $ do
            l <- MV.replicate mx VU.empty
            r <- MV.replicate mx VU.empty
            forM_ [0 .. VU.length pipes - 1] $ \p -> do
              let (a, b) = pipes VU.! p
              MV.modify l (`VU.snoc` p) a
              when (a /= b) $ MV.modify r (`VU.snoc` p) b
            (,) <$> V.freeze l <*> V.freeze r
          go port curr used = foldl' (\cur -> VU.foldl' g cur . (V.! port)) curr [as, bs]
              where g m p | used VU.! p = m
                          | otherwise = max m $ go (a + b - port) (f curr pipe) $ used VU.// [(p, True)]
                          where pipe@(a, b) = pipes VU.! p

part1 :: String -> Int
part1 = solve (\s (a, b) -> s + a + b) 0

part2 :: String -> Int
part2 = snd . solve (\(l, s) (a, b) -> (l + 1, s + a + b)) (0, 0)
