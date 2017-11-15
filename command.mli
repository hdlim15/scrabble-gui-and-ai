(* [coordinate] represents the coordinate of a board cell. *)
type coordinate = int * int

(* [letter] represents a letter character. *)
type letter = char

(* [move] represents a move when a word is played on the board. *)
type move = {
  word_segment : letter list;
  coordinate : coordinate;
  is_horizontal : bool;
}

(* [InvalidCommand] is raised when an entered command has the wrong number of
 * arguments or is not an understood command. *)
exception InvalidCommand

(* [command] represents a command input by a player. *)
type command =
  | PlaceWord of move (* place a word on the board *)
  | Swap of letter list (* swap tiles with the bag *)
  | Score (* show current score of all players *)
  | Rack (* show my rack of tiles *)
  | Hint (* give a hint to the player *)
  | AddWord of string (* adds word to dictionary *)
  | Help (* tells the user what commands they can use to play the game *)
  | Quit (* quit the game *)

(* [parse str] is the command that represents player input [str]. *)
val parse : string -> command
