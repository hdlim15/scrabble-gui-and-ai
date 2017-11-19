open Trie
open Command

(* [coordinate] represents the coordinate of a board cell *)
type coordinate = int * int

(* [letter] represents a scrabble letter as a char, with an associated score value. *)
type letter = (char * int)

(* [cell] represents a cell of the board. *)
type cell = {
  cell_coord : coordinate;
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
  rack : letter list;
  player_type : player_type;
  order_num : int;
}

(* [state] represents all the information relevant to the state of a game. *)
type state = {
  board : board;
  bag : bag;
  players : player list;
  added_words : string list;
  current_player : player;
}

type init_game_data = {
  num_players : int;
  num_humans : int;
  ai_difficulty : difficulty list;
  human_names : string list;
}

(* [InvalidPlace] is an exception that is raised when a player cannot place a
 * sequence of characters at a coordinate. *)
exception InvalidPlace of string

(* [InvalidSwap] is raised if a letter to be swapped is not in the current
 * player's rack or there are not enough letters in the bag to do the swap. *)
exception InvalidSwap

(* [InvalidAdd] is raised if the word to be added to the custom dictionary contains
 * characters besides those in the alphabet *)
exception InvalidAdd

val init_board : int -> board

val get_points : char -> int

val init_bag : unit -> bag

(* [init_state j] is the initial state of the game *)
val init_state : init_game_data -> state

(* [get_row c st] returns a list of cells representing the row that coordinate
 * [c] lies in.
 * raises: [InvalidPlace] if (fst c) is greater than the number of rows. *)
val get_row : coordinate -> state -> cell list

(* [get_cell_from_coordinate c st] returns the cell at coordinate [c] on the
 * board in [st].
 * raises: [InvalidPlace] if [c] does not correspond to a cell in the board. *)
val get_cell_from_coordinate : coordinate -> state -> cell

(* [cell_is_empty c] returns [true] if cell [c] is empty and returns [false] if
 * there is a [letter] at cell [c]. *)
val cell_is_empty  : cell -> bool

(* [remove c lst] returns lst with the first instance of [c] removed. If [c] is
 * not in [lst], [lst] is returned. *)
val remove : 'a -> 'a list -> 'a list

(* [get_adjacent_word c st is_h] returns a pair option with the adjacent word at
 * coordinate [c] on the board in [st] and the points associated with it. [is_h]
 * determines whether the adjacent word is searched for horizontally or
 * vertically. If the cell at [c] is empty, the empty string is returned with 0
 * points. None is returned if [c] is empty or there are no adjacent characters
 * to [c] in the direction specified by [is_h]. *)
val get_adjacent_word : coordinate -> state -> bool -> coordinate list -> (string*int) option

(* [point_moves m] is the number of points earned by the move [m] *)
val point_moves : Command.move -> int

(* [do' c st] is [st'] if doing command [c] in state [st] results
 * in a new state [st']. *)
val do' : Command.command -> state -> state
