{-# LANGUAGE OverloadedStrings #-}

module Year2021.Day22
    ( part1
    , part2
    ) where

import qualified Data.HashMap.Strict as M
import Data.List (foldl')
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal, signed)

parseCubes :: String -> [(String, Int, Int, Int, Int, Int, Int)]
parseCubes = fromJust . parseMaybe @() cubes
    where int = signed (pure ()) decimal
          cube = (,,,,,,) <$> some letterChar <* " x="
                 <*> int <* ".." <*> int <* ",y="
                 <*> int <* ".." <*> int <* ",z="
                 <*> int <* ".." <*> int
          cubes = cube `sepBy` char '\n'

solve :: Int -> Int -> String -> Int
solve lo hi = sum . map func . M.toList . foldl' go M.empty . parseCubes
    where func ((x0', x1', y0', y1', z0', z1'), s) =
              let (x0, x1) = (max lo x0', min hi x1')
                  (y0, y1) = (max lo y0', min hi y1')
                  (z0, z1) = (max lo z0', min hi z1')
              in max 0 (x1 - x0 + 1) * max 0 (y1 - y0 + 1) * max 0 (z1 - z0 + 1) * s
          go cubes (w, nx0, nx1, ny0, ny1, nz0, nz1) =
              let update = foldl' fun M.empty $ M.toList cubes
                  update' = if w == "off" then update
                            else M.alter (Just . (+1) . fromMaybe 0) (nx0, nx1, ny0, ny1, nz0, nz1) update
              in foldl' (\c (k, v) -> M.alter (Just . (+v) . fromMaybe 0) k c) cubes $ M.toList update'
              where fun upd ((ex0, ex1, ey0, ey1, ez0, ez1), es) =
                        let (x0, x1) = (max nx0 ex0, min nx1 ex1)
                            (y0, y1) = (max ny0 ey0, min ny1 ey1)
                            (z0, z1) = (max nz0 ez0, min nz1 ez1)
                            f v = case v of
                                    Nothing -> Just $ -es
                                    Just v' -> Just $ v' - es
                        in if x0 <= x1 && y0 <= y1 && z0 <= z1
                           then M.alter f (x0, x1, y0, y1, z0, z1) upd
                           else upd

part1 :: String -> Int
part1 = solve (-50) 50

part2 :: String -> Int
part2 = solve minBound maxBound
