{-# LANGUAGE OverloadedStrings #-}
module Day02 where
import Intcode
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
  
