module Day04 where

low = 138307
high = 654504

day04 :: IO()
day04 = do
  let pass = [xs | xs <- [low..high], increasingOnly xs, hasDouble xs]
  print (length pass)

increasingOnly :: Int -> Bool
increasingOnly n = increasingOnly' (show n)

increasingOnly' :: String -> Bool
increasingOnly' [] = True
increasingOnly' (x:[]) = True
increasingOnly' (x:y:xs) = x <= y && (increasingOnly' (y:xs))

hasDouble :: Int -> Bool
hasDouble n = hasDouble' (show n)

hasDouble' :: String -> Bool
hasDouble' [] = False
hasDouble' (x:[]) = False
hasDouble' (x:y:z:xs) = if (x == y) && (y == z)
                        then hasDouble' (dropWhile (\n -> x == n) xs)
                        else (x == y) || (hasDouble' (y:z:xs))
hasDouble' (x:y:xs) = x == y || (hasDouble' (y:xs))
