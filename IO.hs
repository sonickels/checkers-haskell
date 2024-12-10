{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wall #-}

module IO where

import System.IO
-- import System.Directory (doesFileExist)
import Control.Monad
import Text.ParserCombinators.Parsec
import GameState
import Checkers

saveFile :: FilePath -> GameState -> (Int, Int) -> (Int, Int) -> IO ()
saveFile save gameState (x1, y1) (x2, y2) = do
    -- exists <- doesFileExist save
    -- if exists then do putStrLn "Saving current game state."
    --     else do putStrLn "No save exists."
    appendFile save ("(" ++ show (currentPlayer gameState) ++ ", " ++ show (x1, y1) ++ " -> " ++ show (x2, y2) ++ ")\n")

loadFile :: FilePath -> IO GameState
loadFile save = do
    -- exists <- doesFileExist save
    -- if exists
    --     then do
        file <- openFile save ReadMode
        _ <- putStrLn ("Loading save " ++ show file)
        input <- hGetContents file
        let lastLine = last (lines input)
        let newPlayer = read (init (tail lastLine)) :: Player
        let newGameState = initGameState { currentPlayer = newPlayer }
        hClose file
        return newGameState
    -- else do
    --     putStrLn "No save exists."
    --     return initGameState

parseLine :: String -> (Player, (Int, Int), (Int, Int))
parseLine line = 
    let (playerStr, rest) = break (== ',') line
        player = read (init (tail playerStr)) :: Player
        coords = words (drop 2 rest)
        (x1, y1) = read (init (tail (coords !! 0))) :: (Int, Int)
        (x2, y2) = read (init (tail (coords !! 2))) :: (Int, Int)
    in (player, (x1, y1), (x2, y2))

readFileLoop :: FilePath -> IO ()
readFileLoop save = do
            input <- readFile save
            let linesOfFile = lines input
            foldM_ processLine initGameState linesOfFile

processLine :: GameState -> String -> IO GameState
processLine gameState line = do
    let (player, from, to) = parseLine line
    let piece = if player == PlayerBlack then Black else Red
    let _ = movePiece (board gameState) from to piece
    let nextPlayer = switchPlayer (currentPlayer gameState)
    return $ gameState { board = board gameState, currentPlayer = nextPlayer }

testIO :: IO ()
testIO = do
    saveFile "test1.txt" initGameState (2, 2) (3, 3)
    putStrLn "Saved move (2, 2) -> (3, 3)"
    let newPlayer = switchPlayer (currentPlayer initGameState)
    let newGameState = initGameState { currentPlayer = newPlayer, board = board initGameState }
    saveFile "test1.txt" newGameState (5, 1) (4, 2)
    printBoard (board newGameState)
    putStrLn "Saved move (5, 1) -> (4, 2)"

    putStrLn "\nCreating new save."
    saveFile "test2.txt" initGameState (2, 3) (3, 4)
    putStrLn "\n"

    -- retrieveGameState <- loadFile "test1.txt"
    -- let newPlayer2 = switchPlayer (currentPlayer retrieveGameState)
    -- let newGameState2 = retrieveGameState { currentPlayer = newPlayer2 }
    -- saveFile "test1.txt" newGameState2 (4, 4) (5, 5)

    putStrLn "Reading file:"
    readFileLoop "test1.txt"

    -- removeFile "test1.txt"
    -- removeFile "test2.txt"

-- enterMove :: [String] -> IO ()
-- enterMove start = do
--     putStrLn "Enter the position of the piece you'd like to move, e.g. F6."
--     piece <- getLine
--     putStrLn "Enter the position you're moving to, e.g. G5."
--     move <- getLine
--     let start = read piece :: [Char]
--     let end = read move :: [Char]
--     if head start `elem` ['A'..'H'] && start !! 1 `elem` ['1'..'8']
--         && head end`elem` ['A'..'H'] && end !! 1 `elem` ['1'..'8']
--         then do
--             putStrLn "Valid move."
--             saveMove start end
--         else putStrLn "Not a valid move."
        
-- getCounts :: String -> [Int]
-- getCounts = map read . words

-- countsText :: [Int] -> String
-- countsText = unwords . map show

-- saveMove :: [Char] -> [Char] -> IO ()
-- saveMove start end = do
--     putStrLn "Saving move." -- error checking

-- retrieveLine :: IO String
-- retrieveLine = do
--     x <- getChar
--     if x == '\n'
--         then return ""
--         else do
--             y <- retrieveLine
--             return (x:y)

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