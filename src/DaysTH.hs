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
import Data.Word
import Language.Haskell.TH
import System.Path.Glob
import System.IO.Unsafe
import Text.Regex.PCRE.Heavy


class PType a where
    unS :: String -> a
    toS :: a -> String

instance PType String where
    unS = id
    toS = id

instance PType ByteString where
    unS = B.pack
    toS = B.unpack

instance PType Int where
    unS = read
    toS = show

instance PType Text where
    unS = T.pack
    toS = T.unpack

instance PType Word16 where
    unS = read
    toS = show

instance (PType a) => PType (IO a) where
    unS = return . unS
    toS = toS . unsafePerformIO -- Hacky workaround for 2016/Day25 bonus

apply :: (PType a, PType b) => (a -> b) -> String -> String
apply f = toS . f . unS

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
