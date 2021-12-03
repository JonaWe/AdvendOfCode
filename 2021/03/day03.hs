{-
    This is a solution to the day 3 puzzle of the Advent of Code.

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
      binaryInputList = map readBinaryString inputList
      part1 = solution1 binaryInputList
      part2 = solution2 binaryInputList
  putStrLn $ "Solution Day 3 Part 1: " ++ show part1 ++ "."
  putStrLn $ "Solution Day 3 Part 2: " ++ show part2 ++ "."

type BinaryNumber = [Int]

--
-- PART 2
--

solution2 :: [BinaryNumber] -> Int
solution2 input = oxygenGeneratorRating input * scrubberRating input

oxygenGeneratorRating :: [BinaryNumber] -> Int
oxygenGeneratorRating input = hornerScheme (calculateRating input 0 interpretOGR) 0

scrubberRating :: [BinaryNumber] -> Int
scrubberRating input = hornerScheme (calculateRating input 0 interpretSR) 0

calculateRating :: [BinaryNumber] -> Int -> (Int -> Int) -> BinaryNumber
calculateRating [x] _ _ = x
calculateRating input pos interpret = calculateRating filtered (pos + 1) interpret
  where
    currentMax = interpret $ countZerosAndOnesAtPos input pos 0
    filtered = filterAtPos input pos currentMax

filterAtPos :: [BinaryNumber] -> Int -> Int -> [BinaryNumber]
filterAtPos input index number = filter (\current -> current !! index == number) input

interpretSR :: Int -> Int
interpretSR x = if x >= 0 then 0 else 1

interpretOGR :: Int -> Int
interpretOGR x = if x >= 0 then 1 else 0

countZerosAndOnesAtPos :: [BinaryNumber] -> Int -> Int -> Int
countZerosAndOnesAtPos [] _ count = count
countZerosAndOnesAtPos (x : xs) pos count
  | x !! pos == 0 = countZerosAndOnesAtPos xs pos $ count - 1
  | x !! pos == 1 = countZerosAndOnesAtPos xs pos $ count + 1
  | otherwise = countZerosAndOnesAtPos xs pos count

--
-- PART 1
--

solution1 :: [BinaryNumber] -> Int
solution1 input = epsilonRateDecimal * gammaRateDecimal
  where
    epsilonRate = calculateEpsilonRate input
    epsilonRateDecimal = hornerScheme epsilonRate 0
    gammaRate = calculateGammaRate input
    gammaRateDecimal = hornerScheme gammaRate 0

calculateEpsilonRate :: [BinaryNumber] -> BinaryNumber
calculateEpsilonRate x = invertBinaryNumber $ calculateGammaRate x

calculateGammaRate :: [BinaryNumber] -> BinaryNumber
calculateGammaRate x = measurementToBinary $ foldl addMeasurement [] x

measurementToBinary :: [Int] -> BinaryNumber
measurementToBinary [] = []
measurementToBinary (x : xs)
  | x > 0 = 1 : measurementToBinary xs
  | otherwise = 0 : measurementToBinary xs

addMeasurement :: [Int] -> BinaryNumber -> [Int]
addMeasurement added [] = added
addMeasurement [] (x : xs)
  | x == 1 = 1 : addMeasurement [] xs
  | x == 0 = -1 : addMeasurement [] xs
  | otherwise = addMeasurement [] xs
addMeasurement (y : ys) (x : xs)
  | x == 1 = y + 1 : addMeasurement ys xs
  | x == 0 = y - 1 : addMeasurement ys xs
  | otherwise = addMeasurement ys xs

--
-- HELPER METHODS
--

readBinaryString :: String -> BinaryNumber
readBinaryString [] = []
readBinaryString (head : tail)
  | head == '0' = 0 : readBinaryString tail
  | head == '1' = 1 : readBinaryString tail
  | otherwise = readBinaryString tail

invertBinaryNumber :: BinaryNumber -> BinaryNumber
invertBinaryNumber [] = []
invertBinaryNumber (head : tail)
  | head == 0 = 1 : invertBinaryNumber tail
  | head == 1 = 0 : invertBinaryNumber tail
  | otherwise = invertBinaryNumber tail

hornerScheme :: BinaryNumber -> Int -> Int
hornerScheme xs num = foldl (\num x -> x + num * 2) num xs
