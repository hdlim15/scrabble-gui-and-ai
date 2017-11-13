type coordinate = int * int

type letter = char

(* [command] represents a command input by a player. *)
type command =
  | PlaceWord of (* place a word on the board *)
      {
        word : letter list;
        coordinate : coordinate;
        is_horizontal : bool;
      }
  | Swap of letter list (* swap tiles with the bag *)
  | Score (* show current score of all players *)
  | MyTiles (* show my tiles *)
  | BagTiles (* number of tiles left in the bag *)
  | Hint (* give a hint to the player *)
  | AddWord of letter list (* adds word to dictionary *)
  | Quit (* quit teh game *)

let parse str =
  failwith "todo"
