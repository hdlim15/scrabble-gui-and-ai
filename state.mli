open Trie
open Command

(* [coordinate] represents the coordinate of a board cell *)
type coordinate = int * int

(* [letter] represents a char. *)
type letter = char

(* [cell] represents a cell of the board. *)
type cell = {
  coordinate : coordinate;
  letter : letter;
  letter_multiplier : int;
  word_multiplier : int;
}

(* [board] represents the game board. *)
type board = (cell list) list

(* [bag] represents the letter tiles not currently in play. *)
type bag = letter list

(* [difficulty] represents the difficulty of the AI. *)
type difficulty = Easy | Hard

(* [player_type] represents a human player or an AI of a specific difficulty. *)
type player_type = Human | AI of difficulty

(* [player] represents a player and their associated information. *)
type player = {
  name : string;
  score: int;
  letters : letter list;
  player_type : player_type;
}

(* [state] represents all the information relevant to the state of a game. *)
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
