module GameState where

-- Define the two players, PlayerRed and PlayerBlack
data Player = PlayerRed | PlayerBlack deriving (Show, Read, Eq)

-- Define the pieces on the board: Empty, Black, Red
data Piece = Empty | Black | Red | BlackKing | RedKing
  deriving (Show, Eq)

-- Define Position as a pair of integers (row, column)
type Position = (Int, Int)

-- Define Board as a list of rows, where each row is a list of Pieces
type Board = [[Piece]]

-- Define GameState to store the board and the current player
data GameState = GameState
  { board :: Board
  , currentPlayer :: Player
  } deriving (Show)

-- Define the size of the board
boardSize :: Int
boardSize = 8


-- Initialize the board with the starting positions of the pieces
initBoard :: Board
initBoard = [
    [Black, Empty, Black, Empty, Black, Empty, Black, Empty],
    [Empty, Black, Empty, Black, Empty, Black, Empty, Black],
    [Black, Empty, Black, Empty, Black, Empty, Black, Empty],
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
    [Empty, Red, Empty, Red, Empty, Red, Empty, Red],
    [Red, Empty, Red, Empty, Red, Empty, Red, Empty],
    [Empty, Red, Empty, Red, Empty, Red, Empty, Red]
  ]

-- Initialize the game state with the board and the starting player
initGameState :: GameState
initGameState = GameState
  { board = initBoard
  , currentPlayer = PlayerRed  -- PlayerRed starts the game
  }

-- Switch the current player
switchPlayer :: Player -> Player
switchPlayer PlayerRed = PlayerBlack
switchPlayer PlayerBlack = PlayerRed

-- Reset the game to its initial state
resetGame :: GameState
resetGame = initGameState

-- Get the piece at a specific position on the board
getPieceAt :: (Int, Int) -> Board -> Maybe Piece
getPieceAt (x, y) board
  | x < 0 || y < 0 || x >= length board || y >= length (head board) = Nothing
  | otherwise = Just (board !! y !! x)

-- Place a piece at a specific position on the board
placePiece :: (Int, Int) -> Piece -> Board -> Board
placePiece (x, y) newPiece board =
  let updatedRow = replaceInRow (board !! y) x newPiece
  in replaceInBoard board y updatedRow

-- Remove a piece from the board
removePiece :: (Int, Int) -> Board -> Board
removePiece pos = placePiece pos Empty

-- Helper function to replace a piece in a row
replaceInRow :: [Piece] -> Int -> Piece -> [Piece]
replaceInRow row idx newPiece =
  take idx row ++ [newPiece] ++ drop (idx + 1) row

-- Helper function to replace a row in the board
replaceInBoard :: Board -> Int -> [Piece] -> Board
replaceInBoard board idx newRow =
  take idx board ++ [newRow] ++ drop (idx + 1) board
