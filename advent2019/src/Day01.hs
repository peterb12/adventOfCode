{-# LANGUAGE OverloadedStrings #-} 
module Day01 where
import Test.HUnit
import Data.Either
import Parsing
import qualified Data.Text.IO as T

day01 :: IO()
day01 = do 
  massfile <- readFile "src/data/day01.txt"
  massfile2 <- T.readFile "src/data/day01.txt"
  let testParse = parse lModules "" massfile2
  -- There's gotta be a more idiomatic way to deal with the Either here.
  let result = fromRight [] testParse
  putStrLn $ show (sum (map fuelForModuleInclusive result))

--Fuel for module is based on its mass. Specifically, to find the fuel required for 
--a module, take its mass, divide by three, round down, and subtract 2.
fuelForModule :: Integer -> Integer
fuelForModule n 
  | n <= 0 = 0
  | otherwise = max (fromIntegral $ (floor (x / 3)) - 2) 0
    where x = fromIntegral n
testFuel1 = TestCase (assertEqual "For 1969" (fuelForModule 1969) 654)
testFuel2 = TestCase (assertEqual "For 100756" (fuelForModule 100756) 33583)
testFuel3 = TestCase (assertEqual "For 12" (fuelForModule 12) 2)
testFuel4 = TestCase (assertEqual "For 14" (fuelForModule 14) 2)

-- Also take account of the additional fuel.
fuelForModuleInclusive :: Integer -> Integer
fuelForModuleInclusive n
  | n <= 0 = 0
  | otherwise = fuel + (fuelForModuleInclusive fuel)
    where fuel = fuelForModule n
testFuel5 = TestCase (assertEqual "inclusive 14" (fuelForModuleInclusive 14) 2)
testFuel6 = TestCase (assertEqual "inclusive 1969" (fuelForModuleInclusive 1969) 966)
testFuel7 = TestCase (assertEqual "inclusive 100756" (fuelForModuleInclusive 100756) 50346)

fuelTests = TestList [TestLabel "testFuel1" testFuel1, 
            TestLabel "testFuel2" testFuel2, 
            TestLabel "testFuel3" testFuel3, 
            TestLabel "testFuel4" testFuel4,
            TestLabel "testFuel5" testFuel5,
            TestLabel "testFuel6" testFuel6,
            TestLabel "testFuel7" testFuel7]
