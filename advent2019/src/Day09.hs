{-# LANGUAGE OverloadedStrings #-}
module Day09 where
import Intcode
import Parsing
import Data.Either
import Data.List as L
import Data.Text.IO as T


test1 = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
test2 = [1102,34915192,34915192,7,4,7,99,0]
test3 = [104,1125899906842624,99]
test4 = [109, 19, 204, -34, 99] ++ [5..3000]
day09 :: IO()
day09 = do
  memoryfile <- T.readFile "src/data/day09.txt"
  let parsed = parse csvInteger "" memoryfile
  let memory = fromRight [] parsed
  let result@(C mem inp out off) = execute (C memory [1] [] 0) 0
--  let result@(C mem inp out off) = execute (C test4 [] [] 2000) 0
  print (show off)
