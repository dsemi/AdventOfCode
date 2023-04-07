{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

import Control.Monad (when)
import qualified Data.ByteString.Char8 as B
import Data.List (intercalate, sort)
import Data.String.Interpolate
import Data.String.Interpolate.Util
import Data.Text (pack)
import qualified Data.Text.IO as T
import Distribution.PackageDescription (emptyHookedBuildInfo)
import Distribution.Simple
import Distribution.Simple.Command (noExtraFlags)
import FlatParse.Basic
import System.FilePath.Glob
import Text.Printf

buildImportFile = do
  files <- sort . map B.pack <$> glob "src/Year????/Day??.hs"
  let importedModules = foldr fn [] files
      output = pack $ unindent [i|
        {-# LANGUAGE TemplateHaskell #-}
        module Days
            ( problem
            , problems
            ) where

        import DaysTH

        #{intercalate "\n        " importedModules}

        $(buildProbs)
      |]

  contents <- T.readFile "src/Days.hs"
  when (contents /= output) $
       T.writeFile "src/Days.hs" output
    where fn f imp = ("import " ++ module') : imp
              where module' = let (year, day) = parse f
                              in "Year" ++ year ++ ".Day" ++ day
                    parser = (,) <$> ($(string "src/Year") *> some (satisfy isDigit))
                             <*> ($(string "/Day") *> some (satisfy isDigit) <* $(string ".hs"))
                    parse inp = case runParser parser inp of
                                  OK res _ -> res
                                  _ -> error "unreachable"

myUserHooks = simpleUserHooks {
                preBuild = fn
              }
    where fn args _ = do
            buildImportFile
            return emptyHookedBuildInfo

main = defaultMainWithHooks myUserHooks
