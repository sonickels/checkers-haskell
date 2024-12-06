{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wall #-}

module Checkers where
    import System.IO
    import System.Directory (doesFileExist)
    import Control.Monad
    import Text.ParserCombinators.Parsec
    import GameState
    blankGame = initGameState

    saveFile :: FilePath -> GameState -> IO ()
    saveFile save gameState = do
        exists <- doesFileExist save
        if exists then do putStrLn "Saving current game state."
            else do putStrLn "No save exists."
        appendFile save ("(" ++ show (currentPlayer gameState) ++ ")\n")

    loadFile :: FilePath -> IO GameState
    loadFile save = do
        exists <- doesFileExist save
        if exists
            then do
            file <- openFile save ReadMode
            _ <- putStrLn ("Loading save " ++ show file)
            input <- hGetContents file
            let lastLine = last (lines input)
            let newPlayer = read (init (tail lastLine)) :: Player
            let newGameState = initGameState { currentPlayer = newPlayer }
            putStrLn $ "Loaded player: " ++ show newPlayer
            hClose file
            return newGameState
        else do
            putStrLn "No save exists."
            return initGameState

    testIO :: IO ()
    testIO = do
        saveFile "test1.txt" blankGame
        let newPlayer = switchPlayer (currentPlayer blankGame)
        let newGameState = blankGame { currentPlayer = newPlayer }
        putStrLn $ "Switched player to: " ++ show newPlayer
        saveFile "test1.txt" newGameState

        putStrLn $ "Creating new save."
        saveFile "test2.txt" blankGame
        
        retrieveGameState <- loadFile "test1.txt"
        let newPlayer = switchPlayer (currentPlayer retrieveGameState)
        let newGameState = retrieveGameState { currentPlayer = newPlayer }
        putStrLn $ "Switched player to: " ++ show newPlayer
        saveFile "test1.txt" newGameState

    enterMove :: [String] -> IO ()
    enterMove start = do
        putStrLn "Enter the position of the piece you'd like to move, e.g. F6."
        piece <- getLine
        putStrLn "Enter the position you're moving to, e.g. G5."
        move <- getLine
        let start = read piece :: [Char]
        let end = read move :: [Char]
        if head start `elem` ['A'..'H'] && start !! 1 `elem` ['1'..'8']
            && head end`elem` ['A'..'H'] && end !! 1 `elem` ['1'..'8']
            then do
                putStrLn "Valid move."
                saveMove start end
            else putStrLn "Not a valid move."
            
    getCounts :: String -> [Int]
    getCounts = map read . words

    countsText :: [Int] -> String
    countsText = unwords . map show

    saveMove :: [Char] -> [Char] -> IO ()
    saveMove start end = do
        putStrLn "Saving move." -- error checking

    retrieveLine :: IO String
    retrieveLine = do
        x <- getChar
        if x == '\n'
            then return ""
            else do
                y <- retrieveLine
                return (x:y)

-- instance ToJSON GameState where
--     toJSON (GameState board currentPlayer) = object ["board" .= board, "player" .= currentPlayer]

-- main = do  
--         contents <- readFile "test.txt"
--         print . map readInt . words $ contents
-- alternately, main = print . map readInt . words =<< readFile "test.txt"

-- show _
-- when (x) $ do
--         y

-- openFile :: FilePath -> IOMode -> IO Handle
-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
-- openFile "stuff.txt" ReadMode