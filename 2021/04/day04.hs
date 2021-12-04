{-
    This is a solution to the day 4 puzzle of the Advent of Code.

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
  let (firstLine : remaining) = lines input
      -- parsing the input
      drawnNumbers :: [Int]
      drawnNumbers = map read $ split ',' firstLine []

      boardStrings = filter (/= []) $ split "" remaining []

      boards :: [Board]
      boards = readBoardsFromStrings boardStrings

      -- solutions
      part1 = solution1 boards drawnNumbers 1
      part2 = 0 -- TODO implement solution
  putStrLn $ "Solution Day 4 Part 1: " ++ show part1 ++ "."
  putStrLn $ "Solution Day 4 Part 2: " ++ show part2 ++ "."

--
-- TYPES
--

type Board = [[Int]]

--
-- PART 1
--

--solution1 :: [Board] -> [Int] -> Int -> Int
solution1 boards numbers maxNumbers = case currentBoardsWin of
  Nothing -> solution1 boards numbers $ maxNumbers + 1
  Just winnerBoard -> sumAllUnchecked winnerBoard currentNumbers * last currentNumbers
  where
    currentNumbers = take maxNumbers numbers
    currentBoardsWin = checkAllBoards boards currentNumbers

checkAllBoards :: [Board] -> [Int] -> Maybe Board
checkAllBoards [] _ = Nothing
checkAllBoards (currentBoard : remaining) numbers = maybesOr (checkIfBoardWon currentBoard numbers) $ checkAllBoards remaining numbers

checkIfBoardWon :: Board -> [Int] -> Maybe Board
checkIfBoardWon board numbers = maybesOr rows cols
  where
    rows = checkRows board
    cols = checkCols $ length board - 1

    checkRows :: [[Int]] -> Maybe Board
    checkRows [] = Nothing
    checkRows (currentRow : remainingRows) =
      if isWinning
        then Just board
        else checkRows remainingRows
      where
        isWinning = checkWinningRowOrCol currentRow numbers

    checkCols :: Int -> Maybe Board
    checkCols (-1) = Nothing
    checkCols pos =
      if isWinning
        then Just board
        else checkCols $ pos - 1
      where
        currentCol = buildCol board pos
        isWinning = checkWinningRowOrCol currentCol numbers

    buildCol :: [[Int]] -> Int -> [Int]
    buildCol [] _ = []
    buildCol (x : xs) pos = x !! pos : buildCol xs pos

checkWinningRowOrCol :: [Int] -> [Int] -> Bool
checkWinningRowOrCol [] numbers = True
checkWinningRowOrCol (x : xs) numbers = (x `elem` numbers) && checkWinningRowOrCol xs numbers

sumAllUnchecked :: Board -> [Int] -> Int
sumAllUnchecked [] _ = 0
sumAllUnchecked (row : remainingRows) numbers = sum (filter (`notElem` numbers) row) + sumAllUnchecked remainingRows numbers

--
-- HELPER
--

--concatMaybe :: a -> Maybe [a] -> Maybe [a]
--concatMaybe x Nothing = Nothing
--concatMaybe x (Just y) = Just (x : y)

maybesOr :: Maybe a -> Maybe a -> Maybe a
maybesOr Nothing Nothing = Nothing
maybesOr (Just x) _ = Just x
maybesOr _ (Just y) = Just y

readBoardsFromStrings :: [[String]] -> [Board]
readBoardsFromStrings [] = []
readBoardsFromStrings (current : tail) = intBoard : readBoardsFromStrings tail
  where
    splitBoardString boardString = split ' ' boardString []
    stringBoard = map (filter (/= "") . splitBoardString) current
    intBoard :: Board
    intBoard = map (map read) stringBoard

split :: Eq a => a -> [a] -> [a] -> [[a]]
split _ [] current = [current]
split x (y : ys) current
  | x == y = current : split x ys []
  | otherwise = split x ys $ current ++ [y]
