{-
    This is a solution to the day 5 puzzle of the Advent of Code.

    Solution created by Jona Wessendorf.
-}

{-
    This is a solution to the day 5 puzzle of the Advent of Code.

    Solution created by Jona Wessendorf.
-}

import Data.Foldable (Foldable (foldl'))
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  run $ head args

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = lines input
      inputLines = map parseLinseString inputList
      part1 = solution1 inputLines
      part2 = solution2 inputLines
  putStrLn $ "Solution Day 5 Part 1: " ++ show part1 ++ "."
  putStrLn $ "Solution Day 5 Part 2: " ++ show part2 ++ "."

--
-- TYPES
--

type Point = (Int, Int)

type Line = (Point, Point)

type PointCount = (Point, Int)

--
-- PART 2
--

solution2 :: [Line] -> Int
solution2 lines = length dangerousPoints
  where
    pointCount = foldr addPointToPointCount [] points
    dangerousPoints = getDangerousPoints pointCount

    points = concatMap getPointsFromLineWithDiagonal lines

--
-- PART 1
--

solution1 :: [Line] -> Int
solution1 lines = length dangerousPoints
  where
    pointCount = foldr addPointToPointCount [] points
    dangerousPoints = getDangerousPoints pointCount

    points = concat $ map getPointsFromLine lines

getPointsFromLineWithDiagonal :: Line -> [Point]
getPointsFromLineWithDiagonal ((x1, y1), (x2, y2))
  | x1 == x2 && y1 == y2 = [(x1, y1)]
  | x1 == x2 && y2 > y1 = (x1, y2) : getPointsFromLineWithDiagonal ((x1, y1), (x2, y2 - 1))
  | x1 == x2 && y2 < y1 = (x1, y2) : getPointsFromLineWithDiagonal ((x1, y1), (x2, y2 + 1))
  | y1 == y2 && x2 > x1 = (x2, y2) : getPointsFromLineWithDiagonal ((x1, y1), (x2 - 1, y2))
  | y1 == y2 && x2 < x1 = (x2, y2) : getPointsFromLineWithDiagonal ((x1, y1), (x2 + 1, y2))
  | y1 < y2 && x1 < x2 = (x2, y2) : getPointsFromLineWithDiagonal ((x1, y1), (x2 - 1, y2 - 1))
  | y1 < y2 && x1 > x2 = (x2, y2) : getPointsFromLineWithDiagonal ((x1, y1), (x2 + 1, y2 - 1))
  | y1 > y2 && x1 < x2 = (x2, y2) : getPointsFromLineWithDiagonal ((x1, y1), (x2 - 1, y2 + 1))
  | y1 > y2 && x1 > x2 = (x2, y2) : getPointsFromLineWithDiagonal ((x1, y1), (x2 + 1, y2 + 1))
  | otherwise = []

getPointsFromLine :: Line -> [Point]
getPointsFromLine ((x1, y1), (x2, y2))
  | x1 == x2 && y1 == y2 = [(x1, y1)]
  | x1 == x2 = (x1, y2) : getPointsFromLine ((x1, y1), (x2, if y2 > y1 then y2 - 1 else y2 + 1))
  | y1 == y2 = (x2, y2) : getPointsFromLine ((x1, y1), (if x2 > x1 then x2 - 1 else x2 + 1, y2))
  | otherwise = []

addPointToPointCount :: Point -> [PointCount] -> [PointCount]
addPointToPointCount (x, y) [] = [((x, y), 1)]
addPointToPointCount (x, y) (((x2, y2), count) : tail)
  | x == x2 && y == y2 = ((x, y), count + 1) : tail
  | otherwise = ((x2, y2), count) : addPointToPointCount (x, y) tail

getDangerousPoints :: [PointCount] -> [PointCount]
getDangerousPoints = filter (\(_, count) -> count >= 2)

--
-- HELPERS
--

parseLinseString :: String -> Line
parseLinseString input = (parsePointString firstPointString, parsePointString secondPointString)
  where
    spaceIndex = fromMaybe 0 (elemIndex ' ' input)
    firstPointString = take spaceIndex input
    secondPointString = drop (spaceIndex + 4) input

parsePointString :: String -> Point
parsePointString input = (firstNumber, secondNumber)
  where
    commaIndex = fromMaybe 0 (elemIndex ',' input)
    firstNumber :: Int
    firstNumber = read $ take commaIndex input
    secondNumber :: Int
    secondNumber = read $ drop (commaIndex + 1) input