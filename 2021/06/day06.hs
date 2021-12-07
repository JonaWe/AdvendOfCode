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
      part1 = solve inputNumbers 80
      part2 = solve inputNumbers 256
  putStrLn $ "Solution Day 6 Part 1: " ++ show part1 ++ "."
  putStrLn $ "Solution Day 6 Part 2: " ++ show part2 ++ "."

--
-- TYPES
--

type LanternFish = Int

type Population = [Int]

--
-- PART 2
--

solve :: [LanternFish] -> Int -> Int
solve fishes days = sum $ simulateDays initialPopulation days
  where
    initialPopulation = readPopulation fishes $ replicate 9 0

readPopulation :: [LanternFish] -> Population -> Population
readPopulation [] pop = pop
readPopulation (fish : rem) [day0, day1, day2, day3, day4, day5, day6, day7, day8]
  | fish == 0 = readPopulation rem [day0 + 1, day1, day2, day3, day4, day5, day6, day7, day8]
  | fish == 1 = readPopulation rem [day0, day1 + 1, day2, day3, day4, day5, day6, day7, day8]
  | fish == 2 = readPopulation rem [day0, day1, day2 + 1, day3, day4, day5, day6, day7, day8]
  | fish == 3 = readPopulation rem [day0, day1, day2, day3 + 1, day4, day5, day6, day7, day8]
  | fish == 4 = readPopulation rem [day0, day1, day2, day3, day4 + 1, day5, day6, day7, day8]
  | fish == 5 = readPopulation rem [day0, day1, day2, day3, day4, day5 + 1, day6, day7, day8]
  | fish == 6 = readPopulation rem [day0, day1, day2, day3, day4, day5, day6 + 1, day7, day8]
  | fish == 7 = readPopulation rem [day0, day1, day2, day3, day4, day5, day6, day7 + 1, day8]
  | fish == 8 = readPopulation rem [day0, day1, day2, day3, day4, day5, day6, day7, day8 + 1]

simulateDays :: Population -> Int -> Population
simulateDays pop 0 = pop
simulateDays pop remainingDays = simulateDays (simulateDay pop) $ remainingDays - 1

simulateDay :: Population -> Population
simulateDay [day0, day1, day2, day3, day4, day5, day6, day7, day8] = [day1, day2, day3, day4, day5, day6, day7 + day0, day8, day0]

--
-- OLD SOLUTION FOR PART 1
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