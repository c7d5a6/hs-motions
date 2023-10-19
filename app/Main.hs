module Main (main) where

import Lib

main :: IO ()
main = print $ show $ astarBench2 10000
