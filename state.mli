open Trie
open Command

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

(* [init_state j] is the initial state of the game *)
val init_state : Trie.dictionary -> state

(* [do' c st] is [st'] if doing command [c] in state [st] results
 * in a new state [st']. *)
val do' : Command.command -> state -> state
