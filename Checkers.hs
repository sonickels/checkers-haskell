
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE LambdaCase #-}

module Checkers where
import GameState

-- Function to print the board with row and column numbers
printBoard :: [[Piece]] -> IO ()
printBoard board = do
  putStrLn "   0   1   2   3   4   5   6   7"  -- Column headers for better readability
  mapM_ printRow (zip [0..] board)
  where
    printRow (rowIndex, row) = do
      putStr $ show rowIndex ++ " "
      mapM_ (putStr . showPiece) row
      putStrLn ""
    showPiece Empty = "[  ]"
    showPiece Black = "[B ]"
    showPiece Red = "[R ]"
    showPiece BlackKing = "[BK]"
    showPiece RedKing = "[RK]"

-- Adjusted movePiece function to return IO (Maybe [[Piece]])
movePiece :: [[Piece]] -> (Int, Int) -> (Int, Int) -> Piece -> IO (Maybe [[Piece]])
movePiece board (x1, y1) (x2, y2) player
  | pieceAtStart == Empty = return Nothing
  | pieceAtStart /= player && not (isKing pieceAtStart player) = return Nothing
  | not (canMoveDirectionally pieceAtStart (y1, y2)) = return Nothing
  | abs (x2 - x1) == 1 && abs (y2 - y1) == 1 = return $ Just (regularMove board (x1, y1) (x2, y2))
  | isCapture board (x1, y1) (x2, y2) player = do
      let newBoard = capturePiece board (x1, y1) (x2, y2) player
      continueCapture newBoard (x2, y2) player  -- Continue capturing recursively
  | otherwise = return Nothing
  where
    pieceAtStart = board !! y1 !! x1

-- Function to check if a piece is a King
isKing :: Piece -> Piece -> Bool
isKing BlackKing Black = True
isKing RedKing Red = True
isKing _ _ = False

-- Helper function to check if the move direction is valid
canMoveDirectionally :: Piece -> (Int, Int) -> Bool
canMoveDirectionally player (y1, y2)
  | player == Black = y2 > y1
  | player == Red = y2 < y1
  | player == BlackKing || player == RedKing = True
  | otherwise = False

-- Regular move (no capture)
regularMove :: [[Piece]] -> (Int, Int) -> (Int, Int) -> [[Piece]]
regularMove board (x1, y1) (x2, y2) =
  let pieceToMove = board !! y1 !! x1
      newRow1 = replaceInRow (board !! y1) x1 Empty
      newRow2 = replaceInRow (board !! y2) x2 pieceToMove
  in replaceInBoard (replaceInBoard board y1 newRow1) y2 newRow2

-- Capturing a piece
capturePiece :: [[Piece]] -> (Int, Int) -> (Int, Int) -> Piece -> [[Piece]]
capturePiece board (x1, y1) (x2, y2) player =
  let pieceToMove = board !! y1 !! x1
      (captureX, captureY) = ((x1 + x2) `div` 2, (y1 + y2) `div` 2)
      newRow1 = replaceInRow (board !! y1) x1 Empty
      newRow2 = replaceInRow (board !! y2) x2 pieceToMove
      updatedBoard = replaceInBoard (replaceInBoard board y1 newRow1) y2 newRow2
  in replaceInBoard updatedBoard captureY (replaceInRow (updatedBoard !! captureY) captureX Empty)

-- Function to check if a move is a capture
isCapture :: [[Piece]] -> (Int, Int) -> (Int, Int) -> Piece -> Bool
isCapture board (x1, y1) (x2, y2) player =
  let opponent = case player of
        Black -> [Red, RedKing]  -- Black can capture both Red and RedKing
        Red -> [Black, BlackKing]  -- Red can capture both Black and BlackKing
        BlackKing -> [Red, RedKing]  -- BlackKing can capture both Red and RedKing
        RedKing -> [Black, BlackKing]  -- RedKing can capture both Black and BlackKing
        _ -> []
      middleX = (x1 + x2) `div` 2
      middleY = (y1 + y2) `div` 2
      middlePiece = board !! middleY !! middleX
  in abs (x2 - x1) == 2 && abs (y2 - y1) == 2 && middlePiece `elem` opponent

-- Find valid capture moves for a player, only captures where there is an opponent followed by an empty space
findCaptures :: [[Piece]] -> (Int, Int) -> Piece -> [(Int, Int)]
findCaptures board (x, y) player = concatMap (findCapture board (x, y) player) validDirections
  where
    validDirections = case player of
      Black -> [(1, 1), (-1, 1)]           -- Black moves forward (downward)
      Red -> [(1, -1), (-1, -1)]           -- Red moves forward (upward)
      BlackKing -> [(1, 1), (-1, 1), (1, -1), (-1, -1)] -- Kings can move in all directions
      RedKing -> [(1, 1), (-1, 1), (1, -1), (-1, -1)]
      _ -> []


-- Check a specific direction for possible captures
checkDirection :: [[Piece]] -> (Int, Int) -> Piece -> (Int, Int) -> [(Int, Int)]
checkDirection board (x, y) player (dx, dy) = findCapture board (x, y) player (dx, dy)

-- Check if the coordinates are valid (within the bounds of the board)
isValidCoord :: Int -> Int -> Bool
isValidCoord x y = x >= 0 && x < boardSize && y >= 0 && y < boardSize

-- Check if the move is a valid capture
findCapture :: [[Piece]] -> (Int, Int) -> Piece -> (Int, Int) -> [(Int, Int)]
findCapture board (x, y) player (dx, dy) =
  let opponent = case player of
                    Black -> [Red, RedKing]
                    Red -> [Black, BlackKing]
                    BlackKing -> [Red, RedKing]
                    RedKing -> [Black, BlackKing]
                    _ -> []
      -- Iteratively check along the diagonal for captures
      checkCaptures currX currY seenOpponent =
        if not (isValidCoord currX currY) then []  -- Out of bounds
        else
          case board !! currY !! currX of
            p | p `elem` opponent && not seenOpponent ->
                let nextX = currX + dx
                    nextY = currY + dy
                in checkCaptures nextX nextY True  -- Found an opponent, continue checking
            Empty | seenOpponent -> [(currX, currY)]  -- Found a valid capture square
            _ -> []  -- Path blocked or invalid
  in checkCaptures (x + dx) (y + dy) False

continueCapture :: [[Piece]] -> (Int, Int) -> Piece -> IO (Maybe [[Piece]])
continueCapture board (x, y) player = do
  printBoard board
  let moreCaptures = findCaptures board (x, y) player
  if null moreCaptures
    then return $ Just board  -- No more captures available
    else do
      putStrLn $ "Additional captures available: " ++ show moreCaptures
      putStrLn "Enter coordinates for the next capture (x, y): "
      x2 <- readLn
      y2 <- readLn
      if (x2, y2) `elem` moreCaptures
        then do
          -- Perform the capture
          let newBoard = capturePiece board (x, y) (x2, y2) player
          continueCapture newBoard (x2, y2) player
        else do
          putStrLn "Invalid capture coordinates. Please try again."
          continueCapture board (x, y) player

-- Promotion to King
promoteToKing :: [[Piece]] -> [[Piece]]
promoteToKing board =
  zipWith (\rowIndex row ->
    if rowIndex == 0 then map (\piece -> if piece == Red then RedKing else piece) row
    else if rowIndex == boardSize - 1 then map (\piece -> if piece == Black then BlackKing else piece) row
    else row
  ) [0..] board

-- Get a move from the user
getMove :: IO (Int, Int, Int, Int)
getMove = do
  putStrLn "Enter the coordinates of the piece you want to move (x1, y1); x first then y: "
  x1 <- readLn
  y1 <- readLn
  putStrLn "Enter the coordinates of the target destination (x2, y2): "
  x2 <- readLn
  y2 <- readLn
  return (x1, y1, x2, y2)

playGame :: [[Piece]] -> Piece -> IO ()
playGame board player = do
  printBoard board
  putStrLn $ "\nPlayer " ++ show player ++ "'s turn!"
  
  -- Get the move input from the player
  (x1, y1, x2, y2) <- getMove
  
  -- Perform the move
  moveResult <- movePiece board (x1, y1) (x2, y2) player
  case moveResult of
    Just newBoard -> do
      let updatedBoard = promoteToKing newBoard
          
      -- Check for a winner after the move
      winner <- checkWinner updatedBoard player
      if winner then
        return ()  -- Game ends if there's a winner
      else do
        let moreCaptures = findCaptures updatedBoard (x2, y2) player
        if null moreCaptures
          then do
            putStrLn "Move successful!"
            let nextPlayer = if player == Black || player == BlackKing then Red else Black
            playGame updatedBoard nextPlayer  -- Continue to the next player's turn
          else do
            putStrLn "Additional captures are required!"
            continueCapture updatedBoard (x2, y2) player >>= \case
              Just finalBoard -> do
                let nextPlayer = if player == Black || player == BlackKing then Red else Black
                playGame finalBoard nextPlayer  -- Continue to the next player's turn after capturing
              Nothing -> playGame updatedBoard player  -- If capture is invalid, stay with the current player
    Nothing -> do
      putStrLn "Invalid move! Please try again."
      playGame board player  -- Retry the move if invalid



-- Function to check if the game has been won
checkWinner :: [[Piece]] -> Piece -> IO Bool
checkWinner board player = do
  -- Define the opponent and opponentKing based on the current player
  let opponent = case player of
                    Black -> Red
                    Red -> Black
                    BlackKing -> RedKing
                    RedKing -> BlackKing
                    _ -> error "Invalid player"

      opponentKing = case player of
                        Black -> RedKing
                        Red -> BlackKing
                        _ -> error "Invalid player"
    
      -- Check if the opponent has no pieces left
      opponentHasPieces = any (\row -> any (\p -> p == opponent || p == opponentKing) row) board

  -- If the opponent has no pieces left, the current player wins
  if not opponentHasPieces then do
    putStrLn $ "Player " ++ show player ++ " wins! The opponent has no pieces left."
    return True
  else do
    return False  -- The game continues if the opponent has pieces left



-- Main function
--main :: IO ()
--main = playGame createBoard Red
