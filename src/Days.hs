{-# LANGUAGE TemplateHaskell #-}
module Days
    ( problems
    ) where

import DaysTH

import Year2015.Day01
import Year2015.Day02
import Year2015.Day03
import Year2015.Day04
import Year2015.Day05
import Year2015.Day06
import Year2015.Day07
import Year2015.Day08
import Year2015.Day09
import Year2015.Day10
import Year2015.Day11
import Year2015.Day12
import Year2015.Day13
import Year2015.Day14
import Year2015.Day15
import Year2015.Day16
import Year2015.Day17
import Year2015.Day18
import Year2015.Day19
import Year2015.Day20
import Year2015.Day21
import Year2015.Day22
import Year2015.Day23
import Year2015.Day24
import Year2015.Day25
import Year2016.Day01
import Year2016.Day02
import Year2016.Day03
import Year2016.Day04
import Year2016.Day05
import Year2016.Day06
import Year2016.Day07
import Year2016.Day08
import Year2016.Day09
import Year2016.Day10
import Year2016.Day11
import Year2016.Day12
import Year2016.Day13
import Year2016.Day14
import Year2016.Day15
import Year2016.Day16
import Year2016.Day17
import Year2016.Day18
import Year2016.Day19
import Year2016.Day20
import Year2016.Day21
import Year2016.Day22
import Year2016.Day23
import Year2016.Day24
import Year2016.Day25
import Year2017.Day01
import Year2017.Day02
import Year2017.Day03
import Year2017.Day04
import Year2017.Day05
import Year2017.Day06
import Year2017.Day07
import Year2017.Day08
import Year2017.Day09
import Year2017.Day10
import Year2017.Day11
import Year2017.Day12
import Year2017.Day13
import Year2017.Day14
import Year2017.Day15
import Year2017.Day16
import Year2017.Day17
import Year2017.Day18
import Year2017.Day19
import Year2017.Day20
import Year2017.Day21
import Year2017.Day22
import Year2017.Day23
import Year2017.Day24
import Year2017.Day25

$(buildProbs)
