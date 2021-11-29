{-
    This is a solution to the day 13 puzzle of the Advent of Code.

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
      part1 = input -- TODO implement solution
      part2 = input -- TODO implement solution
  putStrLn $ "Solution Day 13 Part 1: " ++ show part1 ++ "."
  putStrLn $ "Solution Day 13 Part 2: " ++ show part2 ++ "."