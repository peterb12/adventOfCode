{-# LANGUAGE OverloadedStrings #-}
module Day02 where
import Parsing
import qualified Data.Text.IO as T
import Data.Either
import Debug.Trace
import Control.Lens

day02 :: IO()
day02 = do 
  memoryfile <- T.readFile "src/data/day02.txt"
  let memory = fromRight [] (parse csvInt "" memoryfile)
  let final = search 19690720 memory 0 0
  putStrLn $ show final

-- execute: Given a memory map and a program counter, execute the instruction
-- at that program counter.
-- Assumes no invalid input. This may have to change for part 2...
execute :: [Int] -> Int -> [Int]
execute memory pc
  | memory !! pc == 99 = memory -- If we find '99', we are done.
  | memory !! pc == 1  = execute (memory & (element dest) .~ (x + y)) next
  | memory !! pc == 2  = execute ( memory & (element dest) .~ (x * y)) next
  | otherwise = traceShow "FATAL ERROR" $ memory
  where x    = memory !! (memory !! ((fromIntegral pc) + 1))
        y    = memory !! (memory !! ((fromIntegral pc) + 2))
        dest = memory !! ((pc + 3))
        next = pc + 4

-- If the searched for target is not available, this will never terminate
-- and you will be sad.
search :: Int -> [Int] -> Int -> Int -> Int
search target mem x y
  | result !! 0 == target = ((100 * x) + y)
  | otherwise =
    if (x == 99)
    then (search target mem 0 (y + 1))
    else (search target mem (x + 1) y)
    where result = execute mutatedMem 0
          mutatedMem = (mem & (element 1) .~ x) & (element 2) .~ y
      
  
