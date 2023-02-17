{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module DaysTH
    ( apply
    , buildProbs
    , PType
    ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Complex.Cyclotomic
import Data.Functor.Identity
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Mod
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (KnownNat)
import Language.Haskell.TH
import Linear.V2
import Linear.V3
import System.FilePath.Glob
import Text.Megaparsec
import Text.Megaparsec.Char


class PType a where
    un :: Text -> a
    to :: a -> IO Text

instance (Read a, Show a) => PType a where
    un = read . un
    to = to . show

instance {-# OVERLAPPING #-} PType Cyclotomic where
    un = undefined
    to = to . show

instance {-# OVERLAPPING #-} (PType a) => PType [a] where
    un = map un . T.lines
    to = fmap T.unlines . mapM to

instance {-# OVERLAPPING #-} (PType a, PType b) => PType (Either a b) where
    un = Right . un
    to = \case
         Right v -> to v
         Left  v -> to v

instance {-# OVERLAPPING #-} (KnownNat n) => PType (Mod n) where
    un = fromInteger . un
    to = to . unMod

instance {-# OVERLAPPING #-} PType Text where
    un = id
    to = pure

instance {-# OVERLAPPING #-} PType String where
    un = T.unpack . un
    to = to . T.pack

instance {-# OVERLAPPING #-} PType ByteString where
    un = B.pack . T.unpack . un
    to = to . T.pack . B.unpack

instance {-# OVERLAPPING #-} (PType a) => PType (IO a) where
    un = pure . un
    to = (>>= to)

instance {-# OVERLAPPING #-} (PType a) => PType (Identity a) where
    un = pure . un
    to = to . runIdentity

instance {-# OVERLAPPING #-} (PType a) => PType (Maybe a) where
    un = Just . un
    to = to . fromJust

instance {-# OVERLAPPING #-} (PType a) => PType (a, a) where
    un = undefined
    to (a, b) = T.intercalate "," <$> mapM to [a, b]

instance {-# OVERLAPPING #-} (PType a) => PType (V2 a) where
    un = undefined
    to (V2 a b) = T.intercalate "," <$> mapM to [a, b]

instance {-# OVERLAPPING #-} (PType a) => PType (a, a, a) where
    un = undefined
    to (a, b, c) = T.intercalate "," <$> mapM to [a, b, c]

instance {-# OVERLAPPING #-} (PType a) => PType (V3 a) where
    un = undefined
    to (V3 a b c) = T.intercalate "," <$> mapM to [a, b, c]

apply :: (PType a, PType b) => (a -> b) -> Text -> IO Text
apply f = to . f . un

buildProbs :: Q [Dec]
buildProbs = do
  pFiles <- fmap sort $ runIO $ glob "src/Year????/Day??.hs"
  let parseP :: String -> Q (Integer, Exp)
      parseP x = do
        let (year, day) = fromJust $ parseMaybe parser x
            moduleName  = "Year" ++ year ++ ".Day" ++ day
            part1       = moduleName ++ ".part1"
            part2       = moduleName ++ ".part2"
        entry <- [e|(read day , ( apply $(nm part1)
                                , apply $(nm part2)))|]
        pure (read year, entry)
          where parser :: Parsec () String (String, String)
                parser = (,) <$> (string "src/Year" *> some digitChar)
                         <*> (string "/Day" *> some digitChar <* string ".hs")
                nm = varE . mkName
  [d|problems :: [(Int, [(Int, (Text -> IO Text, Text -> IO Text))])]
     problems = $(ListE . map toLit . M.toAscList . foldr accProbs M.empty <$> mapM parseP pFiles)

     problem :: Int -> Int -> Maybe (Text -> IO Text, Text -> IO Text)
     problem year day = lookup year problems >>= lookup day|]
    where accProbs (year, prob) acc = M.insertWith (++) year [prob] acc
          toLit (a, b) = TupE [ Just (LitE (IntegerL a))
                              , Just (ListE b)
                              ]
