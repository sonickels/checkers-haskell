The classic two-player checkers game in Haskell. The external libraries that we used were QuickCheck and Containers.

Main.hs - responsible for the game rules and dictating the rest of the modules

GameState.hs - defines structure and state changes for the board and pieces

Checkers.hs - handles the game processes, including if a piece is captured, a piece upgrades into a king, etc.

IO.hs - takes care of saving / loading the game
