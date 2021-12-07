{-
    This is a solution to the day 7 puzzle of the Advent of Code.

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
      parsedInput :: [Int]
      parsedInput = map read (split ',' (head inputList) [])
      part1 = solution1 parsedInput
      part2 = 0
  putStrLn $ "Solution Day 7 Part 1: " ++ show part1 ++ "."
  putStrLn $ "Solution Day 7 Part 2: " ++ show part2 ++ "."

--
-- PART 1
--

solution1 :: [Int] -> Int
solution1 input = distance
  where
    minAltitude = minimum input
    maxAltitude = maximum input
    allAltitudes = getAllAltitudes input minAltitude maxAltitude
    (bestAltitude, distance) = getBestAltitude allAltitudes (minAltitude, maxAltitude * length input)

getAllAltitudes :: [Int] -> Int -> Int -> [(Int, Int)]
getAllAltitudes positions min max
  | max >= min = (max, distanceToNumber positions max 0) : getAllAltitudes positions min (max - 1)
  | otherwise = []

getBestAltitude :: [(Int, Int)] -> (Int, Int) -> (Int, Int)
getBestAltitude [] min = min
getBestAltitude ((alt, dis) : tail) (mAlt, mDis)
  | dis < mDis = getBestAltitude tail (alt, dis)
  | otherwise = getBestAltitude tail (mAlt, mDis)

distanceToNumber :: [Int] -> Int -> Int -> Int
distanceToNumber [] _ distance = distance
distanceToNumber (x : xs) avg distance = distanceToNumber xs avg $ distance + abs (avg - x)

--
-- HELPERS
--

--average :: (Real a, Fractional b) => [a] -> b
--average xs = realToFrac (sum xs) / fromIntegral (length xs)

split :: Eq a => a -> [a] -> [a] -> [[a]]
split _ [] current = [current]
split x (y : ys) current
  | x == y && null current = split x ys []
  | x == y = current : split x ys []
  | otherwise = split x ys $ current ++ [y]