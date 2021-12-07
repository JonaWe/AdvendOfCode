{-
    This is a solution to the day 6 puzzle of the Advent of Code.

    Solution created by Jona Wessendorf.
-}

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  run $ head args

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = lines input
      inputNumbers :: [LanternFish]
      inputNumbers = map read $ split ',' (head inputList) []
      part1 = solution1 inputNumbers 80
      part2 = 0
  putStrLn $ "Solution Day 6 Part 1: " ++ show part1 ++ "."
  putStrLn $ "Solution Day 6 Part 2: " ++ show part2 ++ "."

--
-- TYPES
--

type LanternFish = Int

--
-- PART 1
--

solution1 :: [LanternFish] -> Int -> Int
solution1 fishes 0 = length fishes
solution1 fishes remainingDays = solution1 (updateFishList fishes 0) (remainingDays - 1)

updateFishList :: [LanternFish] -> Int -> [LanternFish]
updateFishList [] newFishes = replicate newFishes 8
updateFishList (currentFish : tail) newFishes
  | currentFish == 0 = 6 : updateFishList tail (newFishes + 1)
  | otherwise = currentFish - 1 : updateFishList tail newFishes

--
-- HELPERS
--

split :: Eq a => a -> [a] -> [a] -> [[a]]
split _ [] current = [current]
split x (y : ys) current
  | x == y && null current = split x ys []
  | x == y = current : split x ys []
  | otherwise = split x ys $ current ++ [y]