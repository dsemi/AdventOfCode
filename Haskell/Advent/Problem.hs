module Advent.Problem where

import Text.Regex.PCRE.Heavy (mkRegexQQ)
import Text.Regex.PCRE.Light (dotall)

data Problem = Pure (String -> Int)
             | PureS (String -> String)
             | Impure (String -> IO Int)

redotall = mkRegexQQ [dotall]
