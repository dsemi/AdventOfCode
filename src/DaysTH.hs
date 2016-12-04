module DaysTH
( buildProbs
) where

import Utils

import qualified Data.HashMap.Strict as M
import Data.List (foldl')
import Data.Maybe
import Language.Haskell.TH
import System.Path.Glob
import Text.Megaparsec (digitChar, parseMaybe, some, string, noneOf)
import Text.Megaparsec.String

data Problem = Problem { module' :: String
                       , year :: Integer
                       , day :: Integer
                       , part1 :: Name
                       , part2 :: Name
                       }


problemPathPrefixes :: [String]
problemPathPrefixes = [ "src/Year2015/Day??.hs"
                      , "src/Year2016/Day??.hs"
                      ]


buildProbs :: Q [Dec]
buildProbs = do
  pFiles <- runIO $ concat <$> mapM glob problemPathPrefixes
  let parser :: Parser Problem
      parser = do
        string "src/Year"
        year <- some (noneOf "/")
        string "/Day"
        day <- some digitChar
        string ".hs"
        let moduleName = "Year" ++ year ++ ".Day" ++ day
        return $ Problem moduleName
                   (read year)
                   (read day)
                   (mkName $ moduleName ++ ".part1")
                   (mkName $ moduleName ++ ".part2")
      ps :: [Problem]
      ps = map (fromJust . parseMaybe parser) pFiles
  return [ FunD (mkName "problems")
           [ Clause [] (NormalB (ListE (map toLit . M.toList $ foldl' accProbs M.empty ps))) []
           ]
         ]
    where accProbs acc p = M.insertWith (++) (year p) (buildProb p) acc
          buildProb p = [TupE [ LitE (IntegerL (day p))
                              , TupE [ VarE (part1 p)
                                     , VarE (part2 p)]]]
          toLit (a, b) = TupE [ LitE (IntegerL a)
                              , ListE b]
