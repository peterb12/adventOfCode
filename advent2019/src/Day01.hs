module Day01 where
import Test.HUnit
import Debug.Trace   

day01 :: IO()
day01 = do 
  massfile <- readFile "src/data/day01.txt"
  let allFuel = sum (map fuelForModuleInclusive (map (read::String->Double) (lines massfile)))
  putStrLn $ show allFuel

--Fuel for module is based on its mass. Specifically, to find the fuel required for 
--a module, take its mass, divide by three, round down, and subtract 2.
fuelForModule :: Double -> Double
fuelForModule n 
  | n <= 0 = 0
  | otherwise = max (fromIntegral $ (floor (n / 3)) - 2) 0.0
testFuel1 = TestCase (assertEqual "For 1969" (fuelForModule 1969) 654.0)
testFuel2 = TestCase (assertEqual "For 100756" (fuelForModule 100756) 33583.0)
testFuel3 = TestCase (assertEqual "For 12" (fuelForModule 12) 2.0)
testFuel4 = TestCase (assertEqual "For 14" (fuelForModule 14) 2.0)

-- Also take account of the additional fuel.
fuelForModuleInclusive :: Double -> Double
fuelForModuleInclusive n
  | n <= 0 = 0
  | otherwise = fuel + (fuelForModuleInclusive fuel)
    where fuel = fuelForModule n
testFuel5 = TestCase (assertEqual "inclusive 14" (fuelForModuleInclusive 14) 2.0)
testFuel6 = TestCase (assertEqual "inclusive 1969" (fuelForModuleInclusive 1969) 966.0)
testFuel7 = TestCase (assertEqual "inclusive 100756" (fuelForModuleInclusive 100756) 50346.0)

fuelTests = TestList [TestLabel "testFuel1" testFuel1, 
            TestLabel "testFuel2" testFuel2, 
            TestLabel "testFuel3" testFuel3, 
            TestLabel "testFuel4" testFuel4,
            TestLabel "testFuel5" testFuel5,
            TestLabel "testFuel6" testFuel6,
            TestLabel "testFuel7" testFuel7]
