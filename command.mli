type coordinate = int * int

type move = {
    word : string;
    coordinate : coordinate;
    is_horizontal : bool;
  }

(* [command] represents a command input by a player. *)
type command =
  | PlaceWord of move (* place a word on the board *)
  | Swap of string (* swap tiles with the bag *)
  | Score (* show current score of all players *)
  | MyTiles (* show my tiles *)
  | BagTiles (* number of tiles left in the bag *)
  | Hint (* give a hint to the player *)
  | AddWord of string (* adds word to dictionary *)
  | Quit (* quit teh game *)

(* [parse str] is the command that represents player input [str]. *)
val parse : string -> command

