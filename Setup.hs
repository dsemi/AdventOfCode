{-# LANGUAGE QuasiQuotes #-}

import Control.Monad (when)
import Data.List (intercalate, sort)
import Data.String.Interpolate
import Data.String.Interpolate.Util
import Data.Text (pack)
import qualified Data.Text.IO as T
import Distribution.PackageDescription (emptyHookedBuildInfo)
import Distribution.Simple
import Distribution.Simple.Command (noExtraFlags)
import System.FilePath.Glob
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Printf

buildImportFile = do
  files <- sort <$> glob "src/Year????/Day??.hs"
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
              where module' = let Just (year, day) = parseMaybe parser f
                              in "Year" ++ year ++ ".Day" ++ day
                    parser :: Parsec () String (String, String)
                    parser = (,) <$> (string "src/Year" *> some digitChar)
                             <*> (string "/Day" *> some digitChar <* string ".hs")

myUserHooks = simpleUserHooks {
                preBuild = fn
              }
    where fn args _ = do
            buildImportFile
            return emptyHookedBuildInfo

main = defaultMainWithHooks myUserHooks
