{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wall #-}

module IO where
import Text.ParserCombinators.Parsec
import Checkers
import GameState

-- Saves one move in the format "Player, (x1, y1) -> (x2, y2)""
saveMove :: FilePath -> Piece -> (Int, Int) -> (Int, Int) -> IO ()
saveMove filePath player (x1, y1) (x2, y2) = do
    putStrLn "Saving current game state."
    appendFile filePath (show player ++ " " ++ show (x1, y1) ++ " -> " ++ show (x2, y2) ++ "\n")

-- Reads a move in the format "Player (x1, y1) -> (x2, y2)"
parseMove :: String -> Either ParseError (Piece, (Int, Int), (Int, Int))
parseMove input = parse moveParser "" input
    where
    moveParser = do
        piece <- pieceParser
        spaces
        (x1, y1) <- coordParser
        spaces
        _ <- string "->"
        spaces
        (x2, y2) <- coordParser
        return (piece, (x1, y1), (x2, y2))
    pieceParser = (string "Black" >> return Black)
              <|> (string "Red" >> return Red)
    coordParser = do
        _ <- char '(' 
        x <- many1 digit
        _ <- char ','
        y <- many1 digit
        _ <- char ')'
        return (read x, read y)

-- Saves a list of captures in the format "(x, y); (x, y);..."
saveCaptures :: FilePath -> [(Int, Int)] -> IO ()
saveCaptures filePath captures = do
    let formattedCaptures = concatMap (\(x, y) -> "(" ++ show x ++ ", " ++ show y ++ ") ") captures
    writeFile filePath formattedCaptures

-- Reads a list of captures in the format "(x, y); (x, y);..."
parseCaptures :: String -> Either ParseError [(Int, Int)]
parseCaptures input = parse captures "" input
  where
    captures = sepBy cap spaces
    cap = do
        _ <- char '('
        x <- many1 digit
        _ <- char ','
        spaces
        y <- many1 digit
        _ <- char ')'
        return (read x, read y)

readFileLoop :: FilePath -> IO ()
readFileLoop filePath = do
    content <- readFile filePath
    let linesOfFile = lines content
    putStrLn $ "Reading file: " ++ filePath

testCaptures :: IO ()
testCaptures = do
    let captures = [(2, 3), (4, 5), (6, 7)]
    saveCaptures "captures.txt" captures
    content <- readFile "captures.txt"
    mapM_ (print . parseCaptures) (lines content)

testMoves :: IO ()
testMoves = do
    saveMove "moves.txt" Black (2, 0) (3, 1)
    saveMove "moves.txt" Red (1, 5) (2, 4)
    content <- readFile "moves.txt"
    let moves = lines content
    mapM_ (print . parseMove) moves

testLoop :: IO ()
testLoop = do
    saveMove "loop.txt" Black (2, 0) (3, 1)
    let captures = [(2, 3), (4, 5), (6, 7)]
    saveCaptures "loop.txt" captures
    saveMove "loop.txt" Red (1, 5) (2, 4)
    content <- readFile "loop.txt"
    putStrLn $ "Content: " ++ content
    readFileLoop "loop.txt"
