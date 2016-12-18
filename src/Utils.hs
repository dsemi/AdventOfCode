{-# LANGUAGE MultiWayIf #-}

module Utils where

import UtilsTH

import Control.Lens ((%=), use)
import Control.Monad.Extra (whenM)
import Control.Monad.State
import Data.Either
import Data.Hashable (Hashable)
import Data.HashMap.Strict ((!), HashMap)
import qualified Data.HashMap.Strict as M
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List (minimumBy, tails)
import Data.Ord (comparing)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Lexer
import Text.Megaparsec.String

findAllInts :: (Integral a) => String -> Either (ParseError (Token String) Dec) [a]
findAllInts = ((map fromInteger) <$>) . parse parser ""
    where parser :: Parser [Integer]
          parser = many $ try $ searchAll $ signed (return ()) integer


searchAll :: Parser a -> Parser a
searchAll p = let parser = try p <|> (anyChar *> parser) in parser

findAll :: Parser a -> String -> [a]
findAll parser = rights . map (parse parser "") . init . tails

aStar :: (Eq a, Hashable a) => a -> (a -> Bool) -> (a -> Int) -> (a -> [a]) -> Maybe (Int, a)
aStar start isFinished heuristic neighbors = evalState search startState
    where startState = AStarState (S.singleton start) S.empty (M.singleton start 0) (M.singleton start (heuristic start))
          search = do
            open <- use openSet
            gs <- use gScore
            fs <- use fScore
            let current = minimumBy (comparing (fs !)) $ S.toList open
            if | S.null open        -> return $ Nothing
               | isFinished current -> return $ Just $ (gs M.! current, current)
               | otherwise          -> do
                     openSet %= S.delete current
                     closedSet %= S.insert current
                     forM_ (neighbors current) $ \neighb -> do
                       whenM (not . S.member neighb <$> use closedSet) $ do
                         gs <- use gScore
                         let tentativeGScore = gs ! current + 1
                         isSm <- isSmaller neighb tentativeGScore
                         when isSm $ do
                           gScore %= M.insert neighb tentativeGScore
                           fScore %= M.insert neighb (tentativeGScore + heuristic neighb)
                     search
          isSmaller :: (Eq a, Hashable a) => a -> Int -> State (AStarState a) Bool
          isSmaller neighb tentGScore = do
            open <- use openSet
            gs <- use gScore
            if not (S.member neighb open) then do
              openSet %= S.insert neighb
              return True
            else return $ tentGScore < M.lookupDefault (maxBound :: Int) neighb gs
