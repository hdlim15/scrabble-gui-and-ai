open Trie
open Command
open Ai

type coordinate = int * int

type letter = char

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
  letters : letter list;
  player_type : player_type;
}

type state = {
  board : board;
  bag : bag;
  players : player list;
}

let init_state dict =
  failwith "todo"

let do' cmd st =
  failwith "todo"
