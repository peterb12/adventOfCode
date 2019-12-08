{-# LANGUAGE OverloadedStrings #-}
module Day08 where
import Data.Char
import Data.Either
import Data.List as L
import Data.List.Split as L
import Parsing
import Data.Text as T
import Data.Text.IO as T

testinput = [1,2,3,4,5,6,7,8,9,0,1,2]
testwidth = 3
testheight = 2

width = 25
height = 6
day08 = do
  file <- T.readFile "src/data/day08.txt"
  let parsed = parse digs "" file
  let alldigits = fromRight [] parsed
  let allnums = Prelude.map digitToInt alldigits
  let chunked = (L.chunksOf (width * height) allnums)
  let minLayer = (L.minimum (L.map stats chunked))
  let flatLayer = L.foldr (\x acc -> (smooshPixels x acc)) (L.take (width * height) (L.repeat 2)) chunked
  Prelude.putStrLn $ show flatLayer

smooshPixels :: [Int] -> [Int] -> [Int]
smooshPixels [] _ = []
smooshPixels _ [] = []
smooshPixels (t:top) (b:bottom) = smooshPixel t b : (smooshPixels top bottom)

smooshPixel :: Int -> Int -> Int
smooshPixel top bottom = if top == 2 then bottom else top

-- Given a list of numbers, return:
-- (number of zeros, (number of 1s * number of 2s))
stats :: [Int] -> (Int, Int)
stats xs = (zeros, result)
  where zeros = L.length $ L.filter (==0) xs
        ones = L.length $ L.filter (==1) xs
        twos = L.length $ L.filter (==2) xs
        result = ones * twos