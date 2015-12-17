module Advent.Problem where

data Problem = Pure (String -> Int)
             | PureS (String -> String)
             | Impure (String -> IO Int)
