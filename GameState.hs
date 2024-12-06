module GameState where

-- Importing Data.Map to represent the game board as a map from positions to pieces.
-- We use `qualified` so we can refer to it specifically as `Map` to avoid name conflicts.
import Data.Map (Map)
import qualified Data.Map as Map

-- Define the two players, Red and Black. The `deriving` clause automatically provides
-- instances for `Show` (to print values) and `Eq` (to compare values).
data Player = Red | Black deriving (Show, Read, Eq)

-- Define the two types of pieces: Regular and King, each belonging to one of the players.
-- This type indicates the type and ownership of each piece.
data Piece = Regular Player | King Player deriving (Show, Read, Eq)

-- Define Position as a pair of integers representing (row, column) on an 8x8 board.
type Position = (Int, Int)

-- Define Board as a map from Position to Maybe Piece. The board uses `Map` from `Data.Map`
-- to associate each (row, column) position with either `Nothing` (an empty square) or
-- `Just Piece` (a square occupied by a piece).
type Board = Map Position (Maybe Piece)

-- Define GameState as a data structure that holds:
-- - `board`: the current state of the board.
-- - `currentPlayer`: the player whose turn it is.
-- The `deriving (Show)` clause allows us to print the GameState for debugging.
data GameState = GameState
  { board :: Board
  , currentPlayer :: Player
  } deriving (Show)

-- Initialize an 8x8 board with the starting pieces for each player.
-- `initBoard` creates the initial board state using `Map.fromList`.
initBoard :: Board
initBoard = Map.fromList [((row, col), initialPiece row col) | row <- [0..7], col <- [0..7]]
  where
    -- `initialPiece` is a helper function that determines the piece at each position.
    -- - Black pieces occupy rows 0, 1, and 2 on alternating squares.
    -- - Red pieces occupy rows 5, 6, and 7 on alternating squares.
    -- - Other squares are empty (Nothing).
    initialPiece row col
      | row < 3 && (row + col) `mod` 2 == 1 = Just (Regular Black) -- Black piece
      | row > 4 && (row + col) `mod` 2 == 1 = Just (Regular Red)   -- Red piece
      | otherwise = Nothing -- Empty square

-- Initialize a new GameState with the starting board and set the starting player to Red.
initGameState :: GameState
initGameState = GameState
  { board = initBoard
  , currentPlayer = Red -- Red typically starts in a game of checkers.
  }

-- Switch to the other player. This function alternates turns between Red and Black.
-- It takes the current player and returns the other player.
switchPlayer :: Player -> Player
switchPlayer Red = Black
switchPlayer Black = Red

-- Reset the game to its initial state.
-- This function simply returns a fresh GameState by calling `initGameState`.
resetGame :: GameState
resetGame = initGameState

-- Get the piece at a given position on the board.
-- This function takes a position and the board, and returns the piece at that position.
-- If the position is empty, it returns `Nothing`.
getPieceAt :: Position -> Board -> Maybe Piece
getPieceAt = Map.findWithDefault Nothing

-- Place a piece at a specified position on the board.
-- This function updates the board by inserting a piece (or empty) at the given position.
placePiece :: Position -> Maybe Piece -> Board -> Board
placePiece = Map.insert

-- Remove a piece from the board at a specified position.
-- This function updates the board by setting the specified position to `Nothing`.
-- It's essentially the same as placing `Nothing` at that position.
removePiece :: Position -> Board -> Board
removePiece pos = Map.insert pos Nothing
