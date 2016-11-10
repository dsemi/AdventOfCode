module Year2015.Days
( buildProbs
) where

import Data.List (sort)
import Data.Maybe
import Language.Haskell.TH
import System.Path.Glob
import Text.Megaparsec (digitChar, parseMaybe, some, string)
import Text.Megaparsec.String


problemPathPrefix :: String
problemPathPrefix = "src/Year2015/Day"

buildProbs :: Q [Dec]
buildProbs = do
  pFiles <- runIO $ glob $ problemPathPrefix ++ "??.hs"
  let parser :: Parser String
      parser = string problemPathPrefix *> some digitChar <* string ".hs"
      ps :: [String]
      ps = map (fromJust . parseMaybe parser) pFiles
  return [ FunD (mkName "problems")
           [ Clause [] (NormalB (ListE (concatMap buildProb ps))) []
           ]
         ]
    where buildProb s = let module' = "Year2015.Day" ++ s
                        in [ TupE [ LitE (IntegerL (read s))
                                  , TupE [ VarE (mkName $ module' ++ ".part1")
                                         , VarE (mkName $ module' ++ ".part2")]]
                           ]
