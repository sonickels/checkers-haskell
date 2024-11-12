{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wall #-}

module Checkers where
    import System.IO ()
    import Control.Monad

enterMove :: IO ()
enterMove = do
    putStrLn "Enter the position of the piece you'd like to move, e.g. F6."
    piece <- getLine
    putStrLn "Enter the position you're moving to, e.g. G5."
    move <- getLine
    let start = read piece :: [Char]
    let end = read move :: [Char]
    if head start `elem` ['A'..'H'] && start !! 1 `elem` ['1'..'8']
        && head end`elem` ['A'..'H'] && end !! 1 `elem` ['1'..'8']
        then putStrLn "Valid move."
        saveMove start end
        else putStrLn "Not a valid move."

saveMove :: [Char] -> [Char] -> IO ()
saveMove start end = do
    putStrLn "Saving move." -- error checking

saveFile :: IO ()
saveFile = do
    putStrLn "Saving current game state."
    writeFile "save.txt" "TEST STRING"
    appendFile "save.txt" "ADDITIONAL STRING"

loadFile :: IO ()
loadFile = do
    if doesFileExist "save.txt"
        file <- openFile "save.txt" ReadMode
        putStrLn "Loading last save."
        input <- hGetContents file
        let summary = (countsText . getCounts) input
        putStrLn summary
        hClose file
    else
        putStrLn "No save exists."

retrieveLine :: IO String
retrieveLine = do x <- getChar
                  if x == '\n'
                    then return ""
                    else do y <- retrieveLine
                            return (x:y)

main = do  
        contents <- readFile "test.txt"
        print . map readInt . words $ contents
-- alternately, main = print . map readInt . words =<< readFile "test.txt"

readInt :: String -> Int
readInt = read

-- show _
-- when (x) $ do
--         y

-- openFile :: FilePath -> IOMode -> IO Handle
-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
-- openFile "stuff.txt" ReadMode