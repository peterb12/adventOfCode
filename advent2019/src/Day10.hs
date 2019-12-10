{-# LANGUAGE OverloadedStrings #-}
module Day10 where
import Parsing
import Data.Either
import Data.List as L
import Data.Text.IO as T
import Text.Megaparsec as P
import Text.PrettyPrint as PP

day10 :: IO()
day10 = do
  sectorfile <- T.readFile "src/data/day10.txt"
  let parsed = P.parse (sector <* eof) [] sectorfile
--  let (Left error) = parsed
    
  let rawmap = fromRight [] parsed
  print rawmap

  -- TODO:
  -- Fold this into a map from (X, Y) -> Boolean
