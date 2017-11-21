type coordinate = int * int

type move = {
  word : char list;
  mv_coord : coordinate;
  is_horizontal : bool;
}

(* [command] represents a command input by a player. *)
type command =
  | PlaceWord of move (* place a word on the board *)
  | Swap of char list (* swap tiles with the bag *)
  | Score (* show current score of all players *)
  | Rack (* show my rack of tiles *)
  | Hint (* give a hint to the player *)
  | AddWord of string (* adds word to dictionary *)
  | Help (* tells the user what commands they can use to play the game *)
  | Quit (* quit the game *)
  | Board (* print current state of board *)
  | Pass (* make no move and end turn *)

exception InvalidCommand

(* [get_chars s] splits [s] into a list of its characters. *)
let get_chars s =
  let rec split_helper s' acc =
    match s' with
    | "" -> acc
    | s'' -> split_helper (String.sub s'' 1 ((String.length s'') - 1))
               (String.get s'' 0 :: acc)
  in List.rev (split_helper s [])

(* [index c] is the 0-based index of [c] in the alphabet.
 * requires: c is a lowercase letter in a..z *)
let index c = Char.code c - 97

(* [get_coordinate s] returns a [coordinate] representing the coordinate string
 * [s] entered by a player.
 * Example: get_coordinate "a0" --> (0,0) *)
let get_coordinate s =
  (index (String.get s 0),
   Pervasives.int_of_string (String.sub s 1 ((String.length s) - 1)))

let parse str =
  let lower_str = String.lowercase_ascii (String.trim str) in
  match lower_str with
  | "quit" -> Quit
  | "help" -> Help
  | "hint" -> Hint
  | "score" -> Score
  | "rack" -> Rack
  | "board" -> Board
  | "pass" -> Pass
  | _ ->
    try
      let words = Str.split (Str.regexp "[ \t]+") lower_str in
      match List.hd words with
      | "add" -> AddWord (List.nth words 1)
      | "swap" -> Swap (get_chars (List.nth words 1))
      | "place" ->
        let coordinate = get_coordinate (List.nth words 2) in
        let is_horizontal = (List.nth words 3) = "horizontal" in
        let move = {word = get_chars (List.nth words 1);
                    mv_coord = coordinate;
                    is_horizontal = is_horizontal} in
        PlaceWord move
      | _ -> raise InvalidCommand
    with
    | exn -> raise InvalidCommand
