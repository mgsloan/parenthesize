{-# LANGUAGE QuasiQuotes #-}
module Main where

import Parenthesize

main = putStrLn $ concat
  [ [parensDemoQ|

1 + 2 * 3 / 4

|], [parensDemoQ|

let pyth a b = a * a + b * b
  in pyth 3 * 2 pyth -1 8

|], [parensDemoQ|

5 + 2 * let x = 50
            y = 90
         in x + y

|], [parensDemoQ|

foo . bar $ do stmt1
               let pat1 = rhs1
                   pat2 = rhs2 / 3 + 1 where pat3 = rhs3
                                             pat4 = rhs4
               stmt

|] ]
