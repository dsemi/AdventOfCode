{-# LANGUAGE FlexibleContexts #-}

module Year2018.Day15
    ( part1
    , part2
    ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.List (foldl', sortBy)
import Data.Ord (comparing)
import qualified Data.HashPSQ as Q
import Linear.V2


type Coord = V2 Int
type Val = (Char, Int)

parseGraph :: String -> Array Coord Val
parseGraph input =
    let inputLines = lines input
        rows = length inputLines
        cols = length $ head inputLines
    in listArray (V2 0 0, V2 (cols-1) (rows-1)) $ map addHp $ concat inputLines
    where addHp x
              | x `elem` "EG" = (x, 200)
              | otherwise = (x, 0)

neighbors :: Coord -> [Coord]
neighbors (V2 y x) = [V2 (y-1) x, V2 y (x-1), V2 y (x+1), V2 (y+1) x]

firstMove :: M.HashMap Coord Coord -> Coord -> Maybe Coord
firstMove path = go Nothing
    where go p pos
              | M.member pos path = go (Just pos) (path M.! pos)
              | otherwise = p

findNextMove :: STArray s Coord Val -> Char -> Coord -> ST s (Maybe Coord)
findNextMove grid enemy c = go (1 :: Int) M.empty S.empty $ Q.minView $ Q.singleton c 0 ()
    where go np path closed = \case
              Nothing -> pure Nothing
              Just (pos, _, _, frontier) -> do
                let neighbs = neighbors pos
                found <- anyM (fmap ((==enemy) . fst) . readArray grid) neighbs
                if found
                then pure $ firstMove path pos
                else do
                  ns <- filterM (\x -> pure (not (S.member x closed) && not (Q.member x frontier))
                                       &&^ fmap ((=='.') . fst) (readArray grid x)) neighbs
                  let closed' = S.insert pos closed
                      path' = M.union path $ M.fromList $ map (,pos) ns
                      (np', frontier') = foldl' (\(p, q) n -> (p+1, Q.insert n p () q))
                                         (np, frontier) ns
                  go np' path' closed' $ Q.minView frontier'

sortByM :: (Monad m, Ord b) => (a -> m b) -> [a] -> m [a]
sortByM f xs = do
  xs' <- sequence $ map (\x -> (x,) <$> f x) xs
  pure $ map fst $ sortBy (comparing snd) xs'

runRound :: STArray s Coord Val -> Int -> Bool -> ST s (Either () ())
runRound grid elfPower allowElfDeath = runExceptT $ do
  units <- lift $ getBounds grid >>= filterM (fmap ((`elem` "EG") . fst) . readArray grid) . range
  forM_ units $ \pos -> do
    v <- lift $ readArray grid pos
    when (fst v `elem` "EG") $ do
      let enemy = if fst v == 'E' then 'G' else 'E'
      pos' <- lift $ findNextMove grid enemy pos >>= \case
                Just p -> do
                  writeArray grid pos ('.', 0)
                  writeArray grid p v
                  pure p
                Nothing -> pure pos
      targets <- lift $ filterM (fmap ((==enemy) . fst) . readArray grid) (neighbors pos')
                 >>= sortByM (fmap snd . readArray grid)
      when (not (null targets)) $ do
        let pwr = if fst v == 'E' then elfPower else 3
            tPos = head targets
        (t, hp) <- lift $ readArray grid tPos
        if hp <= pwr
        then if not allowElfDeath && t == 'E'
             then throwError ()
             else lift $ writeArray grid tPos ('.', 0)
        else lift $ writeArray grid tPos (t, hp - pwr)

outcomeOr :: Int -> STArray s Coord Val -> ST s Int -> ST s Int
outcomeOr c grid m = do
  es <- map fst <$> getElems grid
  if 'E' `notElem` es || 'G' `notElem` es
  then (*c) . sum . map snd <$> getElems grid
  else m

part1 :: String -> Int
part1 input = runST $ thaw (parseGraph input) >>= go 0
    where go :: Int -> STArray s Coord Val -> ST s Int
          go c grid = runRound grid 3 True >> outcomeOr c grid (go (c+1) grid)

part2 :: String -> Int
part2 input = runST $ thaw grid' >>= go 0 3
    where grid' = parseGraph input
          go :: Int -> Int -> STArray s Coord Val -> ST s Int
          go c elfPwr grid =
              runRound grid elfPwr False >>= \case
                Left _ -> thaw grid' >>= go 0 (elfPwr+1)
                Right _ -> outcomeOr c grid $ go (c+1) elfPwr grid
