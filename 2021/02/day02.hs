{-
    This is a solution to the day 2 puzzle of the Advent of Code.

    Solution created by Jona Wessendorf.
-}

import Language.Haskell.TH (Dec (ForeignD))
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  run $ head args

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = lines input
      unparsedInstructions = map words inputList
      instructionList = parseInputList unparsedInstructions
      Point x y = solution1 instructionList (Point 0 0)
      part1 = x * y
      part2 = input -- TODO implement solution
  putStrLn $ "Solution Day 2 Part 1: " ++ show part1 ++ "."
  putStrLn $ "Solution Day 2 Part 2: " ++ show part2 ++ "."

data Instruction = Instruction Direction Int

data Direction = Up | Down | Forward

data Point = Point Int Int

parseInputList :: [[String]] -> [Instruction]
parseInputList ([direction, amount] : tail)
  | direction == "up" = Instruction Up amountNumber : parseInputList tail
  | direction == "down" = Instruction Down amountNumber : parseInputList tail
  | direction == "forward" = Instruction Forward amountNumber : parseInputList tail
  where
    amountNumber = read amount
parseInputList _ = []

solution1 :: [Instruction] -> Point -> Point
solution1 [] direction = direction
solution1 (Instruction Up amount : tail) (Point x y) = solution1 tail (Point x (y - amount))
solution1 (Instruction Down amount : tail) (Point x y) = solution1 tail (Point x (y + amount))
solution1 (Instruction Forward amount : tail) (Point x y) = solution1 tail (Point (x + amount) y)