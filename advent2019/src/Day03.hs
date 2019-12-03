{-# LANGUAGE OverloadedStrings #-}
module Day03 where
import Intcode
import Parsing
import qualified Data.Text.IO as T
import Data.Either
import Data.List
import Data.Map as M
import Debug.Trace


day03 :: IO()
day03 = do 
  inputfile <- T.readFile "src/data/day03.txt"
  let items = fromRight ([],[]) (parse turtles "" inputfile)
  let lineOne = moves [(0,0)] (fst items)
  let lineTwo = traceShow (length lineOne) $ moves [(0,0)] (snd items)
  let mapOne = Data.List.foldr (\x acc -> M.insert x () acc) M.empty lineOne
  let mapTwo = Data.List.foldr (\x acc -> M.insert x () acc) M.empty lineTwo
  let intersection = M.keys $ M.intersection mapOne mapTwo
  -- The answer to part 1:
  let closest = (Data.List.minimum (Data.List.map manhattanFromOrigin intersection))
  -- The answer to part2.
  let stepsOne = Data.List.map (\x -> length (snd (break (==x) lineOne))) intersection
  let stepsTwo = Data.List.map (\x -> length (snd (break (==x) lineTwo))) intersection
  print $ Data.List.minimum (zipWith (+) stepsOne stepsTwo)

moves :: [(Int, Int)] -> [Motion] -> [(Int, Int)]
moves _ [] = []
moves (x:xs) (m:ms) = total
  where newMove = move x m
        total =  moves newMove ms ++ newMove
  
move :: (Int, Int) -> Motion -> [(Int, Int)]
move _ (M _ 0) = []
move pt (M dir mag) =  pointInDir pt (M dir mag) : move pt (M dir (mag - 1))

pointInDir :: (Int, Int) -> Motion -> (Int, Int)
pointInDir (x,y) (M Up mag)    = (x, y+mag)
pointInDir (x,y) (M Down mag)  = (x, y-mag)
pointInDir (x,y) (M Lft mag)   = (x-mag, y)
pointInDir (x,y) (M Rght mag)  = (x+mag, y)

closestDistance :: [(Int, Int)] -> Int
closestDistance xs = minimum $ (Data.List.map manhattanFromOrigin xs)

--Manhattan distance: sum of the absolute values of the differences
--of the coordinates
manhattanFromOrigin :: (Int, Int) -> Int
manhattanFromOrigin = manhattan (0,0)
                            
manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (a, b) (c,d) = let e = abs (a - c)
                             f = abs (b - d) in
                           e + f

                           
