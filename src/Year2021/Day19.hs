{-# LANGUAGE NamedFieldPuns, RankNTypes #-}

module Year2021.Day19
    ( part1
    , part2
    ) where

import Control.Lens
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Extra
import Control.Monad.ST
import Data.Bits
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List (foldl1')
import Data.List.Split (splitOn)
import Data.STRef
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word
import Linear.V3

data Scanner f = Scanner { offset :: f (V3 Int)
                         , min' :: f (V3 Int)
                         , ps :: f [V3 Int] }

hash :: V3 Int -> Word64
hash (V3 x y z) = (fromIntegral x `shiftL` 42) `xor`
                  (fromIntegral y `shiftL` 21) `xor`
                  fromIntegral z

parse :: String -> [Scanner Identity]
parse = map scanner . splitOn "\n\n"
    where scanner = (\v -> Scanner (Identity (V3 0 0 0)) (Identity $ mini v) (Identity v))
                    . map pt . tail . lines
          pt line = let [x, y, z] = splitOn "," line
                    in V3 (read x) (read y) (read z)
          mini = foldl1' (\a b -> min <$> a <*> b)

lns :: Int -> Lens' (V3 Int) Int
lns 0 = _x
lns 1 = _y
lns 2 = _z
lns _ = error "invalid index"

infixl 9 !.
(!.) :: V3 Int -> Int -> Int
pt !. n = pt ^. lns n

infixl 9 !
(!) :: STRef s (V3 Int) -> Int -> ST s Int
pt ! n = (!. n) <$> readSTRef pt

infixl 6 .+.
(.+.) :: (Num a) => ST s a -> ST s a -> ST s a
a .+. b = liftM2 (+) a b
infixl 6 +.
(+.) :: (Num a) => a -> ST s a -> ST s a
a +. b = pure a .+. b

infixl 6 .-.
(.-.) :: (Num a) => ST s a -> ST s a -> ST s a
a .-. b = liftM2 (-) a b
infixl 6 -.
(-.) :: (Num a) => a -> ST s a -> ST s a
a -. b = pure a .-. b

swap :: Int -> Int -> V3 Int -> V3 Int
swap a b pt = pt & (lns a .~ (pt ^. lns b)) & (lns b .~ (pt ^. lns a))

align :: Scanner (STRef s) -> Scanner (STRef s) -> Int -> ST s Bool
align a b aa = flip runContT (pure) $ callCC $ \k -> do
  collision <- MV.new $ 4096 * 6 :: ContT r (ST s) (MV.MVector s Word8)
  lift (readSTRef (ps a)) >>= (mapM_ $ \pa ->
      lift (readSTRef (ps b)) >>= (mapM_ $ \pb -> do
          base <- lift $ newSTRef 0
          ns <- lift $ sequence [ 2048 +. (pb!.0 -. (min' b)!0) .-. (pa!.aa -. (min' a)!aa)
                                , (pb!.0 -. (min' b)!0) .+. (pa!.aa -. (min' a)!aa)
                                , 2048 +. (pb!.1 -. (min' b)!1) .-. (pa!.aa -. (min' a)!aa)
                                , (pb!.1 -. (min' b)!1) .+. (pa!.aa -. (min' a)!aa)
                                , 2048 +. (pb!.2 -. (min' b)!2) .-. (pa!.aa -. (min' a)!aa)
                                , (pb!.2 -. (min' b)!2) .+. (pa!.aa -. (min' a)!aa) ]
          forM_ ns $ \n -> do
            idx <- (+n) <$> lift (readSTRef base)
            MV.modify collision (+1) idx
            whenM ((== 12) <$> lift (MV.read collision idx)) $ do
              let ori = idx `div` 4096
                  axis = ori `div` 2
                  neg = ori `mod` 2 == 1
              n <- lift $ n +. (min' b)!axis .+.
                   (if neg then (min' a)!aa else (-2048) -. (min' a)!aa)
              lift $ modifySTRef' (offset b) (lns aa .~ (if neg then -n else n))
              when (axis /= aa) $ lift $ do
                modifySTRef' (min' b) $ swap aa axis
                modifySTRef' (ps b) $ map (swap aa axis)
              lift $ if neg then do
                modifySTRef' (min' b) $ lns aa %~ (n - 2047 -)
                modifySTRef' (ps b) $ map (lns aa %~ (n -))
              else do
                modifySTRef' (min' b) $ lns aa %~ (subtract n)
                modifySTRef' (ps b) $ map (lns aa %~ (subtract n))
              k True
            lift $ modifySTRef' base (+4096)))
  pure False

bits :: Word64 -> [Int]
bits 0 = []
bits n = countTrailingZeros n : bits (n .&. (n-1))

combine :: String -> (HashSet Word64, [Scanner Identity])
combine input = (S.fromList [ hash p | s <- scanners, p <- runIdentity (ps s) ], scanners)
    where scanners = runST $ do
            scnrs <- mapM (thawScanner) $ parse input
            need <- newSTRef $ ((1 :: Word64) `shiftL` length scnrs) - 2
            let go [] = pure ()
                go (i:is) = do
                  js <- fmap bits (readSTRef need) >>=
                        filterM (\j -> align (scnrs !! i) (scnrs !! j) 0) >>=
                        mapM (\j -> do
                                void $ align (scnrs !! i) (scnrs !! j) 1
                                void $ align (scnrs !! i) (scnrs !! j) 2
                                modifySTRef' need (`complementBit` j)
                                pure j)
                  go $ reverse js ++ is
            go [0]
            mapM freezeScanner scnrs
          thawScanner (Scanner {offset, min', ps}) =
              Scanner <$> newSTRef (runIdentity offset) <*>
                      newSTRef (runIdentity min') <*>
                      newSTRef (runIdentity ps)
          freezeScanner (Scanner {offset, min', ps}) =
              Scanner <$> (Identity <$> readSTRef offset) <*>
                          (Identity <$> readSTRef min') <*>
                          (Identity <$> readSTRef ps)

part1 :: String -> Int
part1 = S.size . fst . combine

part2 :: String -> Int
part2 input = let scanners = snd $ combine input
              in maximum [ sum $ abs $ a' - b' | a <- scanners, b <- scanners
                         , let a' = runIdentity (offset a)
                               b' = runIdentity (offset b) ]
