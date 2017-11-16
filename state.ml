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
  rack : letter list;
  player_type : player_type;
}

type state = {
  board : board;
  bag : bag;
  players : player list;
  added_words : string list;
  current_player : player;
}

let init_state dict =
  failwith "todo"

let point_moves m =
  failwith "todo"

exception InvalidPlace

exception InvalidSwap

(* [place w c is_h] places word segment [w] at coordinate [c] horizontally if
 * [is_h] is true and vertically if [is_h] is false.
 * raises: [InvalidPlace] if one cannot place [w] at coordinate [c]. *)
let place w c is_h =
  failwith ""

(* [swap lst st] removes the letters in [lst] from the current player's rack and
 * replaces them with letters in the bag.
 * returns: the updated [state] after the swap.
 * raises: [InvalidSwap] if a letter in [lst] is not in the current
 * player's rack or there are not enough letters in the bag to do the swap. *)
let swap lst st =
  let player = st.current_player in
  let valid_swap =
    List.fold_left (fun acc l -> acc && List.mem_assoc l player.rack) true lst in
  if List.length st.bag >= List.length lst && valid_swap then
    let remove_chars_rack =
      List.filter (fun (c,_) -> not (List.mem c lst)) player.rack in
    let rec bag_to_rack rack bag =
      if List.length rack = 7 then
        let updated_current_player = {player with rack = rack} in
        let updated_players =
          updated_current_player ::
          (List.filter (fun p -> p.name <> updated_current_player.name) st.players) in
        {st with bag = bag;
                 players = updated_players;
                 current_player = updated_current_player}
      else
        let char_from_bag = List.hd bag in
        let updated_bag = List.tl bag in
        bag_to_rack (char_from_bag :: rack) updated_bag
    in bag_to_rack remove_chars_rack st.bag
  else
    raise InvalidSwap

(* [add_word word st] adds [word] to the [added_words] field of [st] and returns
 * the updated state. *)
let add_word word st =
  let added_words' = word::st.added_words in
  {st with added_words = added_words'}

let do' cmd st =
  match cmd with
  | Swap lst -> swap lst st
  | AddWord s -> add_word s st
  | PlaceWord {word_segment; coordinate; is_horizontal} -> failwith ""
  | _ -> st
