{-# LANGUAGE OverloadedStrings #-}
module Day07 where
import Intcode
import Parsing
import Data.Either
import Data.List as L
import Data.Text.IO as T

simple :: [Int]
simple = [0,1,2,3,4]
phases = L.permutations simple

test1 = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
test2 = [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]
test3 = [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]

amp1 = (C test3 [1,0] [])

input = head phases

day07 :: IO()
day07 = do
  memoryfile <- T.readFile "src/data/day07.txt"
  let parsed = parse csvInt "" memoryfile
  let memory = fromRight [] parsed
  print (L.maximum (amplifiers memory phases))

amplifiers :: [Int] -> [[Int]] -> [Int]
amplifiers _ [] = []
amplifiers mem inputs = o5 ++ amplifiers mem (tail inputs)
  where thisInput = (head inputs)
        (C _ _ o1) = execute (C mem [(thisInput !! 0), 0] []) 0
        (C _ _ o2) = execute (C mem ((thisInput !! 1):o1) []) 0
        (C _ _ o3) = execute (C mem ((thisInput !! 2):o2) []) 0
        (C _ _ o4) = execute (C mem ((thisInput !! 3):o3) []) 0
        (C _ _ o5) = execute (C mem ((thisInput !! 4):o4) []) 0
