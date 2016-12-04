module Year2015.Problem where

import Text.Regex.PCRE.Heavy (mkRegexQQ)
import Text.Regex.PCRE.Light (dotall)

redotall = mkRegexQQ [dotall]
