{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module DaysTH
    ( apply
    , buildProbs
    , input
    , buildProb
    , PType
    , PType'(..)
    , UnalteredString(..)
    ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List (intercalate, sort)
import Data.String.Interpolate
import Data.String.Utils
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Path.Glob
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer


newtype UnalteredString = UnalteredString { unwrap :: String }

class PType' a where
    un' :: String -> a

instance PType' String where
    un' = strip

instance PType' Text where
    un' = T.pack . un'

instance PType' Int where
    un' = read . un'

instance PType' UnalteredString where
    un' = UnalteredString

class PType a where
    un :: String -> a
    to :: a -> IO String

instance (Read a, Show a) => PType a where
    un = read . un
    to = to . show

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

input :: Q Exp
input = do
  moduleName <- loc_module <$> qLocation
  let (year, day) = fromJust $ parseMaybe parser moduleName
      inputFile = [i|inputs/#{year}/input#{day}.txt|]
  inp <- runIO (readFile inputFile)
  pure $ AppE (VarE 'un') (LitE (StringL inp))
    where parser :: Parsec () String (Int, Int)
          parser = (,) <$> (string "Year" *> decimal) <*> (string ".Day" *> decimal)

parseModule :: String -> (Int, Int)
parseModule = fromJust . parseMaybe parser
    where parser :: Parsec () String (Int, Int)
          parser = (,) <$> (string "Year" *> decimal) <*> (string ".Day" *> decimal)

findInput :: (Int, Int) -> IO String
findInput (year, day) = readFile [i|inputs/#{year}/input#{day}.txt|]

buildProb :: Q [Dec]
buildProb = do
  moduleName <- loc_module <$> qLocation
  [d|part1 :: String -> IO String
     part1 = const $ findInput (parseModule moduleName) >>= apply $(nm $ moduleName ++ ".part1'")

     part2 :: String -> IO String
     part2 = const $ findInput (parseModule moduleName) >>= apply $(nm $ moduleName ++ ".part2'") |]
    where nm = varE . mkName
