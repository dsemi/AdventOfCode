{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module DaysTH
    ( apply
    , buildProbs
    , PType
    , UnalteredString(..)
    ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Functor.Identity
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Mod
import Data.List (intercalate, sort)
import Data.String.Utils
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (KnownNat)
import Language.Haskell.TH
import System.Path.Glob
import Text.Megaparsec
import Text.Megaparsec.Char


newtype UnalteredString = UnalteredString { unwrap :: String }

class PType a where
    un :: String -> a
    to :: a -> IO String

instance (Read a, Show a) => PType a where
    un = read . un
    to = to . show

instance {-# OVERLAPPING #-} (PType a, PType b) => PType (Either a b) where
    un = Right . un
    to = \case
         Right v -> to v
         Left  v -> to v

instance {-# OVERLAPPING #-} (KnownNat n) => PType (Mod n) where
    un = fromInteger . un
    to = to . unMod

instance {-# OVERLAPPING #-} PType Text where
    un = T.pack . un
    to = to . T.unpack

instance {-# OVERLAPPING #-} PType String where
    un = strip
    to = pure

instance {-# OVERLAPPING #-} PType ByteString where
    un = B.pack . un
    to = pure . B.unpack

instance {-# OVERLAPPING #-} PType UnalteredString where
    un = UnalteredString
    to = to . unwrap

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
    to (a, b) = intercalate "," <$> mapM to [a, b]

instance {-# OVERLAPPING #-} (PType a) => PType (a, a, a) where
    un = undefined
    to (a, b, c) = intercalate "," <$> mapM to [a, b, c]


apply :: (PType a, PType b) => (a -> b) -> String -> IO String
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
  [d|problems :: [(Int, [(Int, (String -> IO String, String -> IO String))])]
     problems = $(ListE . map toLit . M.toAscList . foldr accProbs M.empty <$> mapM parseP pFiles)

     problem :: Int -> Int -> Maybe (String -> IO String, String -> IO String)
     problem year day = lookup year problems >>= lookup day|]
    where accProbs (year, prob) acc = M.insertWith (++) year [prob] acc
          toLit (a, b) = TupE [ LitE (IntegerL a)
                              , ListE b
                              ]
