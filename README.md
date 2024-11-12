The classic two-player checkers game in Haskell. 


main.hs - responsible for the game rules and dictating the rest of the modules

GameState.hs - defines structure and state changes for the board and pieces

MoveLogic.hs - checks for valid moves and special cases

CaptureLogic.hs - handles what happens when a piece is captured

IO.hs - controls input / output to direct the player and saving / loading the game
