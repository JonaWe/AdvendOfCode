{-
    This is a solution to the day 5 puzzle of the Advent of Code.

    Solution created by Jona Wessendorf.
-}

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
      part2 = 0 -- TODO implement solution
  putStrLn $ "Solution Day 5 Part 1: " ++ show part1 ++ "."
  putStrLn $ "Solution Day 5 Part 2: " ++ show part2 ++ "."

--
-- TYPES
--

type Point = (Int, Int)

type Line = (Point, Point)

type PointCount = (Point, Int)

--
-- PART 1
--

solution1 :: [Line] -> Int
solution1 lines = length dangerousPoints
  where
    pointCount = foldr addPointToPointCount [] points
    dangerousPoints = getDangerousPoints pointCount

    points = concat $ map getPointsFromLine lines

getPointsFromLine :: Line -> [Point]
getPointsFromLine ((x1, y1), (x2, y2))
  | x1 == x2 && y1 == y2 = [(x1, y1)]
  | x1 == x2 = (x1, y2) : getPointsFromLine ((x1, y1), (x2, if y2 > y1 then y2 - 1 else y2 + 1))
  | y1 == y2 = (x2, y2) : getPointsFromLine ((x1, y1), (if x2 > x1 then x2 - 1 else x2 + 1, y2))
  | otherwise = []

addPointToPointCount :: Point -> [PointCount] -> [PointCount]
addPointToPointCount (x, y) pointCount =
  if pointIndex /= -1
    then modifyAt pointIndex (\(point, count) -> (point, count + 1)) pointCount
    else ((x, y), 1) : pointCount
  where
    pointIndex = pointCountContainsPoint pointCount (x, y) 0

pointCountContainsPoint :: [PointCount] -> Point -> Int -> Int
pointCountContainsPoint [] _ _ = -1
pointCountContainsPoint (((x1, y1), _) : tail) (x2, y2) index
  | x1 == x2 && y1 == y2 = index
  | otherwise = pointCountContainsPoint tail (x2, y2) $ index + 1

getDangerousPoints :: [PointCount] -> [PointCount]
getDangerousPoints = filter (\(_, count) -> count >= 2)

--
-- HELPERS
--

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt index fun list = initPart ++ fun current : tailPart
  where
    initPart = take index list
    tailPart = drop (index + 1) list
    current = list !! index

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