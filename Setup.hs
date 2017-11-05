{-# LANGUAGE QuasiQuotes #-}

import Control.Monad (when)
import Data.List (sort)
import Data.String.Interpolate
import Data.String.Interpolate.Util
import Data.String.Utils (join)
import Data.Text (pack)
import qualified Data.Text.IO as T
import Distribution.PackageDescription (emptyHookedBuildInfo)
import Distribution.Simple
import Distribution.Simple.Command (noExtraFlags)
import System.Path.Glob
import Text.Regex.PCRE.Heavy
import Text.Printf

buildImportFile = do
  files <- sort <$> glob "src/Year????/Day??.hs"
  let importedModules = foldr fn [] files
      output = pack $ unindent [i|
        {-# LANGUAGE TemplateHaskell #-}
        module Days
            ( problems
            ) where

        import DaysTH

        #{join "\n        " importedModules}

        $(buildProbs)
      |]

  contents <- T.readFile "src/Days.hs"
  when (contents /= output) $
       T.writeFile "src/Days.hs" output
    where fn f imp = ("import " ++ module') : imp
              where module' = let [year, day] = snd $ head $ scan regex f
                              in "Year" ++ year ++ ".Day" ++ day
                    regex = [re|src/Year(\d+)/Day(\d+).hs|]

myUserHooks = simpleUserHooks {
                preBuild = fn
              }
    where fn args _ = do
            buildImportFile
            return emptyHookedBuildInfo

main = defaultMainWithHooks myUserHooks
