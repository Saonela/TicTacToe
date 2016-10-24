module TicTacToe.Messages.Json
where

{-
message to find out a winner
board:
+-+-+-+
|X| |O|
+-+-+-+
| |X|O|
+-+-+-+
| | |X|
+-+-+-+
-}
message :: String
message = "[[\"x\", 0,  \"y\",  0,   \"v\",  \"x\"],  [\"x\", 1, \"y\",   2,  \"v\",   \"o\"],   [\"x\",  1,  \"y\",   1,   \"v\",   \"x\"],   [\"x\", 0, \"y\",  2, \"v\", \"o\"],   [\"x\",   2,  \"y\", 2,  \"v\", \"x\"]]"
