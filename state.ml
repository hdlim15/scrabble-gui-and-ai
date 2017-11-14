open Trie
open Command

type coordinate = int * int

type letter = (char * int)

type cell = {
  coordinate : coordinate;
  letter : letter;
  letter_multiplier : int;
  word_multiplier : int;
}

type board = (cell list) list

type bag = letter list

type difficulty = Easy | Hard

type player_type = Human | AI of difficulty

type player = {
  name : string;
  score: int;
  tiles : letter list;
  player_type : player_type;
}

type state = {
  board : board;
  bag : bag;
  players : player list;
  added_words : string list;
}

let init_state dict =
  failwith "todo"

let point_moves m =
  failwith "todo"

let do' cmd st =
  failwith "todo"
