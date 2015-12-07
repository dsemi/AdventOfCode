module Advent.Days
( buildProbs
) where

import Data.List (sort)
import Language.Haskell.TH
import System.Path.Glob
import Text.Printf

buildProbs :: Q [Dec]
buildProbs = do
  pFiles <- runIO $ glob "Advent/Day??.hs"
  let ps :: [String]
      ps = map (take 2 . drop 10) $ sort pFiles
  return [ FunD (mkName "problems")
           [ Clause [] (NormalB (ListE (concatMap buildProb ps))) []
           ]
         ]
    where buildProb s = let module' = "Advent.Day" ++ s
                        in [ TupE [ LitE (IntegerL (read s))
                                  , TupE [ VarE (mkName $ module' ++ ".part1")
                                         , VarE (mkName $ module' ++ ".part2")]]
                           ]
