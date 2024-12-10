module GameStateTest where

import Test.QuickCheck
import GameState

-- Define an Arbitrary instance for Player
instance Arbitrary Player where
  arbitrary = elements [PlayerRed, PlayerBlack]

-- Property: Initial board has 12 pieces for each player
prop_initBoardHasCorrectPieceCount :: Bool
prop_initBoardHasCorrectPieceCount =
  let board = initBoard
      blackPieces = length [p | row <- board, p <- row, p == Black]
      redPieces = length [p | row <- board, p <- row, p == Red]
  in blackPieces == 12 && redPieces == 12

-- Property: Switching player twice results in the original player
prop_switchPlayerIsReversible :: Player -> Bool
prop_switchPlayerIsReversible player = switchPlayer (switchPlayer player) == player

-- Run all QuickCheck properties
runQuickCheckTests :: IO ()
runQuickCheckTests = do
  quickCheck prop_initBoardHasCorrectPieceCount
  quickCheck prop_switchPlayerIsReversible
