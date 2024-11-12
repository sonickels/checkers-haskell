-- Define the piece types: Empty, Black, Red
data Piece = Empty | Black | Red
  deriving (Show, Eq)

-- Define the size of the board
boardSize :: Int
boardSize = 8

-- Create the initial board with pieces in place
createBoard :: [[Piece]]
createBoard = [
    [Black, Empty, Black, Empty, Black, Empty, Black, Empty],
    [Empty, Black, Empty, Black, Empty, Black, Empty, Black],
    [Black, Empty, Black, Empty, Black, Empty, Black, Empty],
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
    [Empty, Red, Empty, Red, Empty, Red, Empty, Red],
    [Red, Empty, Red, Empty, Red, Empty, Red, Empty],
    [Empty, Red, Empty, Red, Empty, Red, Empty, Red]
  ]

-- Function to print the board with row and column numbers
printBoard :: [[Piece]] -> IO ()
printBoard board = do
  putStrLn "   0  1  2  3  4  5  6  7"  -- Column headers for better readability
  mapM_ printRow (zip [0..] board)
  where
    printRow (rowIndex, row) = do
      putStr $ show rowIndex ++ " "
      mapM_ (putStr . showPiece) row
      putStrLn ""
    showPiece Empty = "[ ]"
    showPiece Black = "[B]"
    showPiece Red = "[R]"

-- Function to move a piece, including capture, with direction rules
movePiece :: [[Piece]] -> (Int, Int) -> (Int, Int) -> Piece -> Maybe [[Piece]]
movePiece board (x1, y1) (x2, y2) player
  | pieceAtStart == Empty = Nothing  -- Check if there is a piece to move
  | pieceAtStart /= player = Nothing  -- Check if the piece matches the current player
  | not (canMoveDirectionally player (y1, y2)) = Nothing  -- Check if the piece can move in the correct direction
  | abs(x2 - x1) == 1 && abs(y2 - y1) == 1 = Just (regularMove board (x1, y1) (x2, y2))  -- Regular move
  | isCapture board (x1, y1) (x2, y2) player = Just (capturePiece board (x1, y1) (x2, y2) player)  -- Capture move
  | otherwise = Nothing  -- Return Nothing if the move is invalid
  where
    pieceAtStart = board !! y1 !! x1  -- Get the piece at the starting position

-- Helper function to check if the move direction is valid
canMoveDirectionally :: Piece -> (Int, Int) -> Bool
canMoveDirectionally player (y1, y2)
  | player == Black = y2 > y1  -- Black can only move down (towards increasing y)
  | player == Red = y2 < y1    -- Red can only move up (towards decreasing y)
  | otherwise = False

-- Helper function for regular move (without capture)
regularMove :: [[Piece]] -> (Int, Int) -> (Int, Int) -> [[Piece]]
regularMove board (x1, y1) (x2, y2) =
  let pieceToMove = board !! y1 !! x1  -- Get the piece to move from the starting position
      newRow1 = replaceInRow (board !! y1) x1 Empty  -- Row where the piece is removed from
      newRow2 = replaceInRow (board !! y2) x2 pieceToMove  -- Row where the piece is added to
  in replaceInBoard (replaceInBoard board y1 newRow1) y2 newRow2

-- Helper function for capturing a piece
capturePiece :: [[Piece]] -> (Int, Int) -> (Int, Int) -> Piece -> [[Piece]]
capturePiece board (x1, y1) (x2, y2) player =
  let pieceToMove = board !! y1 !! x1  -- Get the piece to move from the starting position
      opponent = if player == Black then Red else Black
      (captureX, captureY) = ((x1 + x2) `div` 2, (y1 + y2) `div` 2)  -- The position of the captured piece
      -- Remove the piece from the starting position
      newRow1 = replaceInRow (board !! y1) x1 Empty  
      -- Remove the captured piece
      newRow2 = replaceInRow (board !! y2) x2 pieceToMove
      -- Remove opponent's piece from the captured position
      newBoard = replaceInBoard (replaceInBoard board y1 newRow1) y2 newRow2
  in replaceInBoard newBoard captureY (replaceInRow (newBoard !! captureY) captureX Empty)  -- Remove captured piece

-- Helper function to replace a piece in a specific row
replaceInRow :: [Piece] -> Int -> Piece -> [Piece]
replaceInRow row idx newPiece =
  take idx row ++ [newPiece] ++ drop (idx + 1) row

-- Helper function to replace a row in the board
replaceInBoard :: [[Piece]] -> Int -> [Piece] -> [[Piece]]
replaceInBoard board idx newRow =
  take idx board ++ [newRow] ++ drop (idx + 1) board

-- Function to check if the move is a capture
isCapture :: [[Piece]] -> (Int, Int) -> (Int, Int) -> Piece -> Bool
isCapture board (x1, y1) (x2, y2) player =
  let opponent = if player == Black then Red else Black
      middleX = (x1 + x2) `div` 2
      middleY = (y1 + y2) `div` 2
      middlePiece = (board !! middleY) !! middleX
  in abs (x2 - x1) == 2 && abs (y2 - y1) == 2 && middlePiece == opponent

-- Function to get a move from the user
getMove :: IO (Int, Int, Int, Int)
getMove = do
  putStrLn "Enter the coordinates of the piece you want to move (x1, y1); x first then y: "
  x1 <- readLn
  y1 <- readLn
  putStrLn "Enter the coordinates of the target destination (x2, y2); x first then y: "
  x2 <- readLn
  y2 <- readLn
  return (x1, y1, x2, y2)

-- Game loop to allow player moves
playGame :: [[Piece]] -> Piece -> IO ()
playGame board player = do
  printBoard board
  putStrLn $ "\nPlayer " ++ show player ++ "'s turn!"
  
  -- Get the user's move
  (x1, y1, x2, y2) <- getMove
  
  -- Try to move the piece
  let moveResult = movePiece board (x1, y1) (x2, y2) player
  
  case moveResult of
    Just newBoard -> do
      -- If move is successful, print the updated board
      printBoard newBoard
      putStrLn "Move successful!"
      
      -- Switch to the next player's turn
      let nextPlayer = if player == Black then Red else Black
      playGame newBoard nextPlayer  -- Continue with the next player's turn

    Nothing -> do
      -- If the move is invalid, notify the player and ask them to try again
      putStrLn "Invalid move! Please try again."
      playGame board player  -- Continue with the same player's turn

-- Main function to start the game
main :: IO ()
main = do
  let initialBoard = createBoard
  playGame initialBoard Black  -- Black starts the game
