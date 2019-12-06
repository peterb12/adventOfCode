{-# LANGUAGE OverloadedStrings #-}
module Day06 where
import Parsing
import Data.Either
import Data.List as L
import Data.Map as M
import Data.Text as T
import Data.Text.IO as T

import Debug.Trace

-- The answer to part 1 is the sum of distances to the root of the tree.
day06 :: IO()
day06 = do
  orbitfile <- T.readFile "src/data/day06.txt"
  let parsed = parse orbits "" orbitfile
  let orbits = fromRight [("bad","bad")] parsed
  let orbitMap = M.fromList orbits
  let distances = L.foldr (\x acc -> (distance orbitMap x) + acc) 0 (M.keys orbitMap)
  let meToRoot = pathToRoot orbitMap "YOU"
  let santaToRoot = pathToRoot orbitMap "SAN"
  let common = L.head (intersect meToRoot santaToRoot)
  let foo = fst $ L.break (== "5Y6") meToRoot
  let bar = fst $ L.break (== "HQX") santaToRoot
  print (L.length (foo ++ bar) - 3)
  print (foo ++ bar)
  print distances

distance :: (Map T.Text T.Text) -> T.Text -> Int
distance _ "COM" = 0
distance map item = 1 + (distance map (map ! item))
--distance map item = traceShow item $ 1

pathToRoot :: (Map T.Text T.Text) -> T.Text -> [T.Text]
pathToRoot _ "COM" = ["COM"]
pathToRoot map item = item : pathToRoot map (map ! item)