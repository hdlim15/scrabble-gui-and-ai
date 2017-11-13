open Trie
open Command

type coordinate = int * int

type cell = {
  coordinate : coordinate;
  letter : char;
  letter_multiplier : int;
  word_multiplier : int;
}

type board = (cell list) list

type bag = char list

type player = {
  name : string;
  score: int;
  letters : char list;
  is_human : bool;
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
