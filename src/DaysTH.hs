{-# LANGUAGE FlexibleInstances, QuasiQuotes, TemplateHaskell #-}

module DaysTH
    ( buildProbs
    ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as M
import Data.Int
import Data.Text (Text)
import Data.Word
import qualified Data.Text as T
import Language.Haskell.TH
import System.Path.Glob
import Text.Regex.PCRE.Heavy


class PType a where
    un :: Text -> a
    to :: a -> IO Text

instance (PType a) => PType (IO a) where
    un = return . un
    to = (>>= to)

instance PType String where
    un = T.unpack
    to = return . T.pack

instance PType ByteString where
    un = B.pack . T.unpack
    to = return . T.pack . B.unpack

instance PType Text where
    un = id
    to = return

instance PType Int where
    un = read . T.unpack
    to = return . T.pack . show

instance PType Int8 where
    un = read . T.unpack
    to = return . T.pack . show

instance PType Int16 where
    un = read . T.unpack
    to = return . T.pack . show

instance PType Int32 where
    un = read . T.unpack
    to = return . T.pack . show

instance PType Int64 where
    un = read . T.unpack
    to = return . T.pack . show

instance PType Word where
    un = read . T.unpack
    to = return . T.pack . show

instance PType Word8 where
    un = read . T.unpack
    to = return . T.pack . show

instance PType Word16 where
    un = read . T.unpack
    to = return . T.pack . show

instance PType Word32 where
    un = read . T.unpack
    to = return . T.pack . show

instance PType Word64 where
    un = read . T.unpack
    to = return . T.pack . show

apply :: (PType a, PType b) => (a -> b) -> Text -> IO Text
apply f = to . f . un

buildProbs :: Q [Dec]
buildProbs = do
  pFiles <- runIO $ glob "src/Year????/Day??.hs"
  let parse :: String -> Q (Integer, Exp)
      parse x = do
        let [year, day] = snd . head $ scan r x
            moduleName  = "Year" ++ year ++ ".Day" ++ day
            part1       = moduleName ++ ".part1"
            part2       = moduleName ++ ".part2"
        entry <- [e|(read day :: Integer , ( apply $(nm part1)
                                           , apply $(nm part2)))|]
        return (read year, entry)
          where r = [re|src/Year(\d+)/Day(\d+).hs|]
                nm = varE . mkName
  [d|problems = $(ListE . map toLit . M.toList . foldr accProbs M.empty <$> mapM parse pFiles)|]
    where accProbs (year, prob) acc = M.insertWith (++) year [prob] acc
          toLit (a, b) = TupE [ LitE (IntegerL a)
                              , ListE b
                              ]
