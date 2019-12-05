{-# LANGUAGE OverloadedStrings #-}
module Day05 where
import Intcode
import Parsing

import Data.Either
import Data.Text.IO as T
import Debug.Trace

day05 :: IO()
day05 = do
  memoryfile <- T.readFile "src/data/day05.txt"
  let parsed = parse csvInt "" memoryfile
  let memory = fromRight [] parsed
  let final = execute memory 0
  print $ "last bit of memory is " ++ (show (last final))
