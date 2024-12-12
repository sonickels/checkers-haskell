{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import GameState
import Checkers
import IO

-- Main function to start the game
main :: IO ()
main = do
  putStrLn $ "Would you like to play a new game? (y/n): "
  newGame <- getLine

  if newGame == "y" then do
    putStrLn "Starting new game."
    playGame initBoard Black  -- Black starts the game

  else if newGame == "n" then do
    putStrLn "What file would you like to load?"
    fileName <- getLine
    playGame initBoard Black  -- Black starts the game

  else do
    putStrLn "Invalid input. Please try again."
    main

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
      saveMove "save.txt" player (x1, y1) (x2, y2)
      let updatedBoard = promoteToKing newBoard
          wasRegMove = abs (x2 - x1) == 1  -- Assumes capture moves involve jumping over a square
      -- Check for a winner after the move
      winner <- checkWinner updatedBoard player
      if winner then
        return ()  -- Game ends if there's a winner
      else do
        let moreCaptures = findCaptures updatedBoard (x2, y2) player
        if null moreCaptures || wasRegMove
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