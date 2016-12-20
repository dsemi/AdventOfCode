{-# LANGUAGE TemplateHaskell, MultiWayIf #-}

module DaysTH
( Problem(..)
, buildProbs
, unwrap
) where

import Utils

import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as M
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Path.Glob
import Text.Megaparsec (digitChar, parseMaybe, some, string, noneOf)
import Text.Megaparsec.String
import Debug.Trace

data Problem = StoS (String -> String)
             | StoB (String -> ByteString)
             | StoI (String -> Int)
             | StoT (String -> Text)
             | ItoS (Int -> String)
             | ItoB (Int -> ByteString)
             | ItoI (Int -> Int)
             | ItoT (Int -> Text)
             | BtoS (ByteString -> String)
             | BtoB (ByteString -> ByteString)
             | BtoI (ByteString -> Int)
             | BtoT (ByteString -> Text)
             | TtoS (Text -> String)
             | TtoB (Text -> ByteString)
             | TtoI (Text -> Int)
             | TtoT (Text -> Text)

sToS = StoS
sToB = StoB
sToI = StoI
sToT = StoT
iToS = ItoS
iToB = ItoB
iToI = ItoI
iToT = ItoT
bToS = BtoS
bToB = BtoB
bToI = BtoI
bToT = BtoT
tToS = TtoS
tToB = TtoB
tToI = TtoI
tToT = TtoT

unwrap :: Problem -> String -> String
unwrap (StoS f) = f
unwrap (StoB f) = B.unpack . f
unwrap (StoI f) = show . f
unwrap (StoT f) = T.unpack . f
unwrap (BtoS f) = f . B.pack
unwrap (BtoB f) = B.unpack . f . B.pack
unwrap (BtoI f) = show . f . B.pack
unwrap (BtoT f) = T.unpack . f . B.pack
unwrap (ItoS f) = f . read
unwrap (ItoB f) = B.unpack . f . read
unwrap (ItoI f) = show . f . read
unwrap (ItoT f) = T.unpack . f . read
unwrap (TtoS f) = f . T.pack
unwrap (TtoB f) = B.unpack . f . T.pack
unwrap (TtoI f) = show . f . T.pack
unwrap (TtoT f) = T.unpack . f . T.pack

data ProblemTH = ProblemTH { module' :: String
                           , year :: Integer
                           , day :: Integer
                           , part1 :: String
                           , part2 :: String
                           }

problemPathPrefixes :: [String]
problemPathPrefixes = [ "src/Year2015/Day??.hs"
                      , "src/Year2016/Day??.hs"
                      ]

buildProbs :: Q [Dec]
buildProbs = do
  pFiles <- runIO $ concat <$> mapM glob problemPathPrefixes
  let parser :: Parser ProblemTH
      parser = do
        string "src/Year"
        year <- some (noneOf "/")
        string "/Day"
        day <- some digitChar
        string ".hs"
        let moduleName = "Year" ++ year ++ ".Day" ++ day
        return $ ProblemTH moduleName
                   (read year)
                   (read day)
                   (moduleName ++ ".part1")
                   (moduleName ++ ".part2")
      ps :: [ProblemTH]
      ps = map (fromJust . parseMaybe parser) pFiles
  probs <- map toLit . M.toList <$> foldM accProbs M.empty ps
  return [ FunD (mkName "problems")
           [ Clause [] (NormalB (ListE probs)) []
           ]
         ]
    where accProbs acc p = do
               prob <- buildProb p
               return $ M.insertWith (++) (year p) prob acc
          buildProb p = do
               Just p1Name <- lookupValueName (part1 p)
               Just p2Name <- lookupValueName (part2 p)
               p1Types <- getTypes p1Name
               p2Types <- getTypes p2Name
               let p1Const = probConst p1Types
               let p2Const = probConst p2Types
               return [TupE [ LitE (IntegerL (day p))
                            , TupE [ AppE (VarE p1Const) (VarE p1Name)
                                   , AppE (VarE p2Const) (VarE p2Name)]]]
          toLit (a, b) = TupE [ LitE (IntegerL a)
                              , ListE b]
          getTypes x = do
               VarI _ (AppT (AppT _ (ConT t1)) (ConT t2)) _ <- reify x
               return (t1, t2)
          probConst (t1, t2) = if | t1 == ''String && t2 == ''String -> 'sToS
                                  | t1 == ''String && t2 == ''Int -> 'sToI
                                  | t1 == ''String && t2 == ''ByteString -> 'sToB
                                  | t1 == ''String && t2 == ''Text -> 'sToT
                                  | t1 == ''ByteString && t2 == ''String -> 'bToS
                                  | t1 == ''ByteString && t2 == ''Int -> 'bToI
                                  | t1 == ''ByteString && t2 == ''ByteString -> 'bToB
                                  | t1 == ''ByteString && t2 == ''Text -> 'bToT
                                  | t1 == ''Int && t2 == ''String -> 'iToS
                                  | t1 == ''Int && t2 == ''Int -> 'iToI
                                  | t1 == ''Int && t2 == ''ByteString -> 'iToB
                                  | t1 == ''Int && t2 == ''Text -> 'iToT
                                  | t1 == ''Text && t2 == ''String -> 'tToS
                                  | t1 == ''Text && t2 == ''Int -> 'tToI
                                  | t1 == ''Text && t2 == ''ByteString -> 'tToB
                                  | t1 == ''Text && t2 == ''Text -> 'tToT
                                  | otherwise -> trace (show (t1,t2)) undefined
