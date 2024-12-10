module Main where

import GameState
import Checkers
import IO

-- Main function to start the game
main :: IO ()
main = do
  let initialBoard = createBoard
  playGame initialBoard Black  -- Black starts the game