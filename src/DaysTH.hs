{-# LANGUAGE FlexibleInstances, QuasiQuotes, TemplateHaskell, TypeSynonymInstances #-}

module DaysTH
    ( buildProbs
    ) where

import Utils

import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH
import System.Path.Glob
import Text.Regex.PCRE.Heavy
import Data.Word
import Debug.Trace


class PType a where
    wrap :: String -> a
    unwrap :: a -> String

instance PType String where
    wrap = id
    unwrap = id

instance PType ByteString where
    wrap = B.pack
    unwrap = B.unpack

instance PType Int where
    wrap = read
    unwrap = show

instance PType Text where
    wrap = T.pack
    unwrap = T.unpack

instance PType Word16 where
    wrap = read
    unwrap = show

apply :: (PType a, PType b) => (a -> b) -> String -> String
apply f = unwrap . f . wrap

problemPathPrefixes :: [String]
problemPathPrefixes = [ "src/Year2015/Day??.hs"
                      , "src/Year2016/Day??.hs"
                      ]

buildProbs :: Q [Dec]
buildProbs = do
  pFiles <- runIO $ concat <$> mapM glob problemPathPrefixes
  let parse :: String -> Q (Integer, Exp)
      parse x = do
        let [year, day] = snd . head $ scan r x
            moduleName  = "Year" ++ year ++ ".Day" ++ day
            part1       = moduleName ++ ".part1"
            part2       = moduleName ++ ".part2"
        entry <- [e|(read day, ( apply $(nm part1)
                               , apply $(nm part2)))|]
        return (read year, entry)
          where r = [re|src/Year(\d+)/Day(\d+).hs|]
                nm = varE . mkName
  [d|problems = $(ListE . map toLit . M.toList . foldr accProbs M.empty <$> mapM parse pFiles)|]
    where accProbs (year, prob) acc = M.insertWith (++) year [prob] acc
          toLit (a, b) = TupE [ LitE (IntegerL a)
                              , ListE b
                              ]
