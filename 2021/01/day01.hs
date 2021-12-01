{-
    This is a solution to the day 1 puzzle of the Advent of Code.

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
      intInputList :: [Int]
      intInputList = map read inputList
      part1 = countIncreased intInputList
      part2 = countThreeMeasurementWindowIncreased intInputList
  putStrLn $ "Solution Day 1 Part 1: " ++ show part1 ++ "."
  putStrLn $ "Solution Day 1 Part 2: " ++ show part2 ++ "."

countIncreased :: [Int] -> Int
countIncreased [] = 0
countIncreased [last] = 0
countIncreased (first : second : tail)
  | first < second = 1 + countIncreased (second : tail)
  | otherwise = countIncreased $ second : tail

countThreeMeasurementWindowIncreased :: [Int] -> Int
countThreeMeasurementWindowIncreased [] = 0
countThreeMeasurementWindowIncreased (a : b : c : d : tail)
  | a + b + c < b + c + d = 1 + countThreeMeasurementWindowIncreased (b : c : d : tail)
  | otherwise = countThreeMeasurementWindowIncreased (b : c : d : tail)
countThreeMeasurementWindowIncreased (head : tail) = 0