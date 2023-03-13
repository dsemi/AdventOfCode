{-# LANGUAGE DeriveGeneric, NoFieldSelectors, OverloadedRecordDot #-}

module Year2022.Day22
    ( part1
    , part2
    ) where

import Control.Applicative (liftA2)
import Data.Array.Unboxed
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.List (foldl', tails)
import Data.List.Split (splitOn)
import Data.Maybe
import GHC.Generics (Generic)
import Linear.V2
import Linear.V3
import Linear.Vector
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer

data Face = Top | Bottom | Back | Front | Starboard | Port deriving (Eq)

axis :: Face -> V3 Int
axis = \case
       Top -> unit _z
       Bottom -> -unit _z
       Back -> unit _y
       Front -> -unit _y
       Starboard -> unit _x
       Port -> -unit _x

fallOff :: Face -> V3 Int -> (Face, V3 Int)
fallOff face dir = let dir' = case dir of
                                V3 0 0 1 -> Top
                                V3 0 0 (-1) -> Bottom
                                V3 0 1 0 -> Back
                                V3 0 (-1) 0 -> Front
                                V3 1 0 0 -> Starboard
                                V3 (-1) 0 0 -> Port
                                _ -> error "unreachable"
                   in (dir', -axis face)

rotate :: Face -> V3 Int -> V3 Int
rotate face (V3 x y z) = case face of
                           Top -> V3 (-y) x z
                           Bottom -> V3 y (-x) z
                           Back -> V3 z y (-x)
                           Front -> V3 (-z) y x
                           Starboard -> V3 x (-z) y
                           Port -> V3 x z (-y)

data Dir = R | D | L | U deriving (Enum, Eq, Generic, Show)
instance Hashable Dir

move :: Dir -> V2 Int -> V2 Int
move R (V2 r c) = V2 r (c + 1)
move D (V2 r c) = V2 (r + 1) c
move L (V2 r c) = V2 r (c - 1)
move U (V2 r c) = V2 (r - 1) c

data Instr = Turn Bool | Step Int

data Board = Board (UArray (V2 Int) Char) [Instr] (V2 Int)

newBoard :: String -> Board
newBoard input = Board grid instrs $ head $ filter ((/= ' ') . (grid !)) $ iterate (+ V2 0 1) (V2 0 0)
    where pts = splitOn "\n\n" input
          rows = lines $ head pts
          grid = accumArray (flip const) ' ' (V2 0 0, V2 (length rows - 1) (maximum (map length rows) - 1))
                 [ (V2 r c, v) | (r, row) <- zip [0..] rows
                 , (c, v) <- zip [0..] row ]
          instrs = fromJust $ parseMaybe @() (some parseInstr) $ pts !! 1
          parseInstr = (Step <$> decimal) <|> (Turn . (== 'R') <$> oneOf "LR")

valid :: UArray (V2 Int) Char -> V2 Int -> Bool
valid grid idx = inRange (bounds grid) idx && grid ! idx /= ' '

type Vec = (V2 Int, Dir)

walk :: Board -> (V2 Int -> Dir -> Vec) -> Int
walk (Board grid path topLeft) step = final $ foldl' go (start, R) path
    where start = head $ filter ((== '.') . (grid !)) $ iterate (+ V2 0 1) topLeft
          go (pos, dir) (Turn False) = (pos, toEnum $ (fromEnum dir - 1) `mod` 4)
          go (pos, dir) (Turn True) = (pos, toEnum $ (fromEnum dir + 1) `mod` 4)
          go (pos, dir) (Step n)
              | n == 0 = (pos, dir)
              | grid ! pos' == '#' = (pos, dir)
              | otherwise = go (pos', dir') $ Step $ n-1
              where (pos', dir') = step pos dir
          final (V2 x y, dir) = 1000*(x + 1) + 4*(y + 1) + fromEnum dir

data Edge = Edge { pos :: V2 Int
                 , dir :: Dir
                 , src :: Face
                 , dest :: Face }

cubeEdges :: Board -> HashMap Vec Vec
cubeEdges (Board grid _ topLeft) =
    edgeMap $ go M.empty topLeft U (V3 0 (cubeSize-1) (cubeSize-1)) (V3 0 1 0) Top
    where V2 rows cols = fmap (+1) snd $ bounds grid
          cubeSize = gcd rows cols
          go edges pos@(V2 r c) dir pos3d dir3d face =
              let steps = [ (toEnum $ (fromEnum dir - 1) `mod` 4, rotate face dir3d)
                          , (dir, dir3d)
                          , (toEnum $ (fromEnum dir + 1) `mod` 4, rotate face (-dir3d))]
                  (edgs, (d2, d3):_) = span (\(d, _) -> not $ valid grid (move d pos)) steps
                  ((face', dir3d'), pos3d') =
                      if d2 == D && (r + 1) `mod` cubeSize == 0 ||
                         d2 == R && (c + 1) `mod` cubeSize == 0 ||
                         d2 == U && r `mod` cubeSize == 0 ||
                         d2 == L && c `mod` cubeSize == 0
                      then (fallOff face d3, pos3d)
                      else ((face, d3), pos3d + d3)
                  edges' = foldr (\(d2', d3') -> M.insertWith (++) pos3d $
                                                 [Edge pos d2' face (fst (fallOff face d3'))])
                           edges edgs
                  pos' = move d2 pos
              in if pos' == topLeft then M.elems edges'
                 else go edges' pos' d2 pos3d' dir3d' face'
          edgeMap :: [[Edge]] -> HashMap Vec Vec
          edgeMap edges = M.fromList $ concat
                          [ [ ((a.pos, a.dir), (b.pos, toEnum ((fromEnum (b.dir) + 2) `mod` 4)))
                            , ((b.pos, b.dir), (a.pos, toEnum ((fromEnum (a.dir) + 2) `mod` 4))) ]
                          | pts <- edges
                          , a:bs <- init (tails pts)
                          , b <- bs
                          , a.src == b.dest && b.src == a.dest ]

part1 :: String -> Int
part1 input = walk board f
    where board@(Board grid _ _) = newBoard input
          lims = fmap (+1) $ snd $ bounds grid
          wrap pt = liftA2 mod pt lims
          f pos dir = (pos', dir)
              where pos' = head $ filter (valid grid) $ tail $ iterate (wrap . move dir) pos

part2 :: String -> Int
part2 input = walk board f
    where board@(Board grid _ _) = newBoard input
          edges = cubeEdges board
          f pos dir
              | valid grid pos' = (pos', dir)
              | otherwise = edges M.! (pos, dir)
              where pos' = move dir pos
