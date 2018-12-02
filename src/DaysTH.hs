{-# LANGUAGE FlexibleInstances, QuasiQuotes, TemplateHaskell #-}

module DaysTH
    ( buildProbs
    , input
    , PType'(..)
    , UnalteredString(..)
    ) where

import Control.Monad.IO.Class
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as M
import Data.Int
import Data.String.Interpolate
import Data.String.Utils
import Data.Text (Text)
import Data.Word
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Path.Glob
import Text.Regex.PCRE.Heavy


newtype UnalteredString = UnalteredString { unwrap :: String }

class PType' a where
    un' :: String -> a

instance PType' String where
    un' = strip

instance PType' Int where
    un' = read . un'

instance PType' UnalteredString where
    un' = UnalteredString

class PType a where
    un :: Text -> a
    to :: (MonadIO m) => a -> m Text

instance (PType a) => PType (IO a) where
    un = pure . un
    to = liftIO . (>>= to)

instance PType String where
    un = T.unpack . un
    to = pure . T.pack

instance PType ByteString where
    un = B.pack . T.unpack . un
    to = pure . T.pack . B.unpack

instance PType Text where
    un = T.strip
    to = pure

instance PType UnalteredString where
    un = UnalteredString . T.unpack
    to = pure . T.pack . unwrap

instance PType Int where
    un = read . T.unpack . un
    to = pure . T.pack . show

instance PType Int8 where
    un = read . T.unpack . un
    to = pure . T.pack . show

instance PType Int16 where
    un = read . T.unpack . un
    to = pure . T.pack . show

instance PType Int32 where
    un = read . T.unpack . un
    to = pure . T.pack . show

instance PType Int64 where
    un = read . T.unpack . un
    to = pure . T.pack . show

instance PType Word where
    un = read . T.unpack . un
    to = pure . T.pack . show

instance PType Word8 where
    un = read . T.unpack . un
    to = pure . T.pack . show

instance PType Word16 where
    un = read . T.unpack . un
    to = pure . T.pack . show

instance PType Word32 where
    un = read . T.unpack . un
    to = pure . T.pack . show

instance PType Word64 where
    un = read . T.unpack . un
    to = pure . T.pack . show

apply :: (MonadIO m, PType a, PType b) => (a -> b) -> Text -> m Text
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
        pure (read year, entry)
          where r = [re|src/Year(\d+)/Day(\d+).hs|]
                nm = varE . mkName
  [d|problems :: [(Integer, [(Integer, (Text -> IO Text, Text -> IO Text))])]
     problems = $(ListE . map toLit . M.toList . foldr accProbs M.empty <$> mapM parse pFiles)|]
    where accProbs (year, prob) acc = M.insertWith (++) year [prob] acc
          toLit (a, b) = TupE [ LitE (IntegerL a)
                              , ListE b
                              ]

input :: Q Exp
input = do
  moduleName <- loc_module <$> qLocation
  let [year, day] = map read . snd . head $ scan r moduleName :: [Int]
      inputFile = [i|inputs/#{year}/input#{day}.txt|]
  inp <- runIO (readFile inputFile)
  pure $ AppE (VarE 'un') (LitE (StringL inp))
    where r = [re|Year(\d+)\.Day(\d+)|]
