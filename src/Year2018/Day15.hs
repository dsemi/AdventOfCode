module Year2018.Day15
    ( part1
    , part2
    ) where

import Data.Array
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Sequence (ViewL(..), (><))
import qualified Data.Sequence as Q
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

findNextMove :: Array Coord Val -> Char -> Coord -> Maybe Coord
findNextMove grid enemy c = go M.empty (S.singleton c) $ Q.viewl $ Q.singleton c
    where go _ _ EmptyL = Nothing
          go path closed (pos :< frontier)
              | any ((==enemy) . fst . (grid !)) neighbs = firstMove path pos
              | otherwise = let ns = filter (\x -> not (S.member x closed)
                                                   && fst (grid ! x) == '.') neighbs
                                closed' = foldr S.insert closed ns
                                path' = M.union path $ M.fromList $ map (,pos) ns
                                frontier' = frontier >< Q.fromList ns
                            in go path' closed' $ Q.viewl frontier'
              where neighbs = neighbors pos

data Result = Finished | ElfDied | EndedEarly

runRound :: Array Coord Val -> Int -> Bool -> (Array Coord Val, Result)
runRound arr elfPower allowElfDeath =
    let units = filter ((`elem` "EG") . fst . (arr !)) $ range $ bounds arr
    in go arr (length $ filter ((=='E') . fst) $ elems arr)
           (length $ filter ((=='G') . fst) $ elems arr) units
    where go :: Array Coord Val -> Int -> Int -> [Coord] -> (Array Coord Val, Result)
          go grid _ _ [] = (grid, Finished)
          go grid elves goblins (pos:xs)
              | elves == 0 || goblins == 0 = (grid, EndedEarly)
              | fst v `notElem` "EG" = go grid elves goblins xs
              | otherwise =
                  let (grid', pos') = case findNextMove grid enemy pos of
                                        Just p -> (grid // [(pos, ('.', 0)), (p, v)], p)
                                        Nothing -> (grid, pos)
                      targets = sortBy (comparing (snd . (grid' !)))
                                $ filter ((==enemy) . fst . (grid' !)) (neighbors pos')
                  in if null targets
                     then go grid' elves goblins xs
                     else let pwr = if fst v == 'E' then elfPower else 3
                              tPos = head targets
                              (t, hp) = grid' ! tPos
                          in if hp <= pwr
                             then if not allowElfDeath && t == 'E'
                                  then (grid', ElfDied)
                                  else let elves' = if t == 'E' then elves - 1 else elves
                                           goblins' = if t == 'G' then goblins - 1 else goblins
                                       in go (grid' // [(tPos, ('.', 0))]) elves' goblins' xs
                             else go (grid' // [(tPos, (t, hp - pwr))]) elves goblins xs
              where v = grid ! pos
                    enemy = if fst v == 'E' then 'G' else 'E'

score :: Array Coord Val -> Result -> Int -> Int -> Int
score _ ElfDied _ _ = error "Bad score"
score grid EndedEarly c x = score grid Finished (c-1) x
score grid Finished c x
    | 'E' `notElem` es || 'G' `notElem` es = (*c) $ sum $ map snd $ elems grid
    | otherwise = x
    where es = map fst $ elems grid

part1 :: String -> Int
part1 input = go 1 $ parseGraph input
    where go c grid = score grid' res c $ go (c+1) grid'
              where (grid', res) = runRound grid 3 True

part2 :: String -> Int
part2 input = go 1 3 gridStart
    where gridStart = parseGraph input
          go c elfPwr grid = case runRound grid elfPwr False of
                               (_, ElfDied) -> go 1 (elfPwr+1) gridStart
                               (grid', res) -> score grid' res c $ go (c+1) elfPwr grid'
