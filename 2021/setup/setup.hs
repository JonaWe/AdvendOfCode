import System.Directory (createDirectoryIfMissing, executable, getPermissions, setPermissions)

main :: IO ()
main = do
  template <- readFile "template.hs"
  writeAllDays 1 31 template

writeAllDays :: Int -> Int -> String -> IO ()
writeAllDays currentDay maxDays template
  | currentDay <= maxDays =
    do
      makeDirForDay currentDay
      writeDay currentDay template
      writeAllDays (currentDay + 1) maxDays template
  | otherwise =
    print "Done!"

makeDirForDay :: Int -> IO ()
makeDirForDay day = createDirectoryIfMissing True $ "../" ++ show day

writeDay :: Int -> String -> IO ()
writeDay day template =
  do
    writeFile haskellFilePath $ replaceDay day template
    writeFile inputFilePath "Input goes here!"
    writeFile runFilePath runFileContent
    currentPermissions <- getPermissions runFilePath
    setPermissions runFilePath currentPermissions {executable = True}
  where
    dayString = show day
    dirPath = "../" ++ dayString ++ "/"
    haskellFilePath = dirPath ++ "day" ++ dayString ++ ".hs"
    inputFilePath = dirPath ++ "input.txt"
    runFilePath = dirPath ++ "run.sh"
    runFileContent = "#!/bin/bash\n\nghc day" ++ dayString ++ ".hs\n./day" ++ dayString ++ " input.txt"

replaceDay :: Int -> String -> String
replaceDay day = replace '*' $ show day

replace :: Char -> String -> String -> String
replace _ _ [] = ""
replace toBeReplaced replacement (currentChar : remaining)
  | toBeReplaced == currentChar = replacement ++ replace toBeReplaced replacement remaining
  | otherwise = currentChar : replace toBeReplaced replacement remaining
