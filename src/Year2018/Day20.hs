{-# LANGUAGE TypeApplications, ViewPatterns #-}

module Year2018.Day20
    ( part1
    , part2
    ) where

import Control.Monad.State
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import qualified Data.HashPSQ as Q
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Maybe
import Data.List (foldl')
import Linear.V2
import Text.Megaparsec (between, many, parseMaybe, sepBy, try, (<|>))
import Text.Megaparsec.Char


type Coord = V2 Int

parseEdges :: String -> HashSet (Coord, Coord)
parseEdges = fromJust . parseMaybe @() (evalStateT (between (char '^') (char '$') edges) (V2 0 0))
    where edges = fmap S.unions . many $ try step <|> branch
          step = do
            pos <- get
            d <- oneOf "NSEW"
            let pos' = pos + case d of
                               'N' -> V2 0 (-1)
                               'E' -> V2 1 0
                               'S' -> V2 0 1
                               'W' -> V2 (-1) 0
            put pos'
            S.insert (if pos <= pos' then (pos, pos') else (pos', pos)) <$> edges
          branch = between (char '(') (char ')') $ do
                     pos <- get
                     fmap S.unions . (`sepBy` char '|') $ do
                       put pos
                       edges

edgeMap :: HashSet (Coord, Coord) -> HashMap Coord (HashSet Coord)
edgeMap = M.fromListWith S.union . concatMap (\(a, b) -> [(a, S.singleton b), (b, S.singleton a)])

distsFrom :: Coord -> HashMap Coord (HashSet Coord) -> HashMap Coord Int
distsFrom pos m = go M.empty S.empty $ Q.minView $ Q.singleton pos 0 ()
    where go dists closed = \case
              Nothing -> dists
              Just (coord, succ -> c, _, frontier) ->
                  let neighbs = filter (\x -> not (S.member x closed) && not (Q.member x frontier))
                                $ S.toList $ m M.! coord
                      dists' = M.union dists $ M.fromList $ map (,c) neighbs
                      closed' = S.insert coord closed
                      frontier' = foldl' (\q n -> Q.insert n c () q) frontier neighbs
                  in go dists' closed' $ Q.minView frontier'

getDists :: String -> [Int]
getDists = M.elems . distsFrom (V2 0 0) . edgeMap . parseEdges

part1 :: String -> Int
part1 = maximum . getDists

part2 :: String -> Int
part2 = length . filter (>=1000) . getDists
