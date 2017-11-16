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

type init_game_data = {
  num_players : int;
  num_humans : int;
  ai_difficulty : difficulty list;
  human_names : string list;
}

type state = {
  board : board;
  bag : bag;
  players : player list;
  added_words : string list;
  current_player : player;
}

(* [bag_to_rack r b p st] adds letters from bag [b] to player [p]'s rack [r]
 * until the player has 7 letters on their rack.
 * returns: the updated state after letters are added from [b] to [r]. *)
let rec bag_to_rack r b p st =
  if List.length r = 7 then
    let updated_player = {p with rack = r} in
    let updated_players =
      updated_player ::
      (List.filter (fun p -> p.name <> updated_player.name) st.players) in
    {st with bag = b;
             players = updated_players}
  else
    let i = Random.int (List.length b) in
    let letter_from_bag = List.nth b i in
    let updated_bag = List.remove_assoc (fst letter_from_bag) b in
    bag_to_rack (letter_from_bag :: r) updated_bag p st

(* [get_points c] returns the number of points associated with letter [c]. *)
let get_points c =
  match c with
  | 'a' | 'e' | 'i' | 'l' | 'n'
  | 'o' | 'r' | 's' | 't' | 'u' -> 1
  | 'd' | 'g'                   -> 2
  | 'b' | 'c' | 'm' | 'p'       -> 3
  | 'f' | 'h' | 'v' | 'w' | 'y' -> 4
  | 'k'                         -> 5
  | 'j' | 'x'                   -> 8
  | 'q' | 'z'                   -> 10
  | '*'                         -> 0
  | _ -> failwith "impossible"

(* [init_board n] creates an nxn board.
 * requires: n > 0 *)
let rec init_board n =
  let rec helper n' =
    match n' with
    | 0 -> gen_row 0 n :: []
    | i -> helper (i - 1) @ [gen_row i n] in
  helper (n - 1)
and gen_row row_num len =
  match len with
  | 0 -> []
  | i -> gen_row row_num (len - 1) @ [gen_cell row_num (i-1)]
and gen_cell row_num col_num =
  {coordinate = row_num, col_num;
   letter = (' ', -1);
   letter_multiplier = 1;
   word_multiplier = 1}

(* [init_bag ()] creates a scrabble bag of tiles *)
let rec init_bag () =
  let alphabet = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';
                  'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z';'*'] in
  (* let rec helper lst =
    match lst with
    | [] -> []
    | h::t -> init_letters_of_char h @ helper t in
  helper alphabet *)
  List.flatten (List.map init_letters_of_char alphabet) (*david*)
and init_letters_of_char c =
  match c with
  | 'a' -> num_tiles_of_char 9 'a' 1  | 'b' -> num_tiles_of_char 2 'b' 3
  | 'c' -> num_tiles_of_char 2 'c' 3  | 'd' -> num_tiles_of_char 4 'd' 2
  | 'e' -> num_tiles_of_char 12 'e' 1 | 'f' -> num_tiles_of_char 2 'f' 4
  | 'g' -> num_tiles_of_char 3 'g' 2  | 'h' -> num_tiles_of_char 2 'h' 4
  | 'i' -> num_tiles_of_char 9 'i' 1  | 'j' -> num_tiles_of_char 1 'j' 8
  | 'k' -> num_tiles_of_char 1 'k' 5  | 'l' -> num_tiles_of_char 4 'l' 1
  | 'm' -> num_tiles_of_char 2 'm' 3  | 'n' -> num_tiles_of_char 6 'n' 1
  | 'o' -> num_tiles_of_char 8 'o' 1  | 'p' -> num_tiles_of_char 2 'p' 3
  | 'q' -> num_tiles_of_char 1 'q' 10 | 'r' -> num_tiles_of_char 6 'r' 1
  | 's' -> num_tiles_of_char 4 's' 1  | 't' -> num_tiles_of_char 6 't' 1
  | 'u' -> num_tiles_of_char 4 'u' 1  | 'v' -> num_tiles_of_char 2 'v' 4
  | 'w' -> num_tiles_of_char 2 'w' 4  | 'x' -> num_tiles_of_char 1 'x' 8
  | 'y' -> num_tiles_of_char 2 'y' 4  | 'z' -> num_tiles_of_char 1 'z' 10
  | '*' -> num_tiles_of_char 2 '*' 0  | _ -> failwith "impossible"
and num_tiles_of_char n c p =
  match n with
  | 0 -> []
  | i -> (c, p) :: num_tiles_of_char (n - 1) c p

let rec gen_human_players num names =
  match num, names with
  | 0, [] -> []
  | i, name::t -> {name=name;score=0;rack=[];player_type=Human}
                  :: gen_human_players (num-1) t
  | _ -> failwith "impossible, num = names.length"

let rec gen_ai_players num difficulty =
  match num, difficulty with
  | 0, [] -> []
  | i, diff::t -> {name="AI_"^(string_of_int i);score=0;rack=[];player_type=AI diff}
                  :: gen_ai_players (num-1) t
  | _ -> failwith "impossible, num = difficulty.length"

let init_players init_data =
  let num_players = init_data.num_players in
  let num_humans = init_data.num_humans in
  if num_players = num_humans then
    gen_human_players num_players init_data.human_names
  else
    let human_players = gen_human_players num_humans init_data.human_names in
    let ai_players = gen_ai_players (num_players-num_humans) init_data.ai_difficulty in
    human_players @ ai_players

let rec init_racks players st =
  match players with
  | [] -> st
  | h::t ->
    let st' = bag_to_rack (h.rack) (st.bag) h st in
    init_racks t st'

let init_state init_data =
  let board = init_board 15 in
  let bag = init_bag () in
  let players = init_players init_data in
  let current_player = List.hd players in
  let st = {board=board;
            bag=bag;
            players=players;
            added_words=[];
            current_player=current_player} in
  let st' = init_racks players st in
  {st' with current_player = List.hd st'.players}

let point_moves m =
  failwith "todo"

exception InvalidPlace

exception InvalidSwap

(* [get_row c st] returns a list of cells representing the row that coordinate
 * [c] lies in.
 * raises: [InvalidPlace] if (fst c) is greater than the number of rows. *)
let get_row c st =
  try List.nth st.board (fst c) with
  | exn -> raise InvalidPlace

(* [get_column c st] returns a list of cells representing the column that
 * coordinate [c] lies in.
 * raises: [InvalidPlace] if (snd c) is greater than the number of columns. *)
let get_column c st =
  let col_num = snd c in
  try
    let rec col_helper b acc =
      match b with
      | [] -> acc
      | h::t -> col_helper t (List.nth h col_num :: acc)
    in col_helper st.board []
  with
  | exn -> raise InvalidPlace

(* [get_cell_from_coordinate c st] returns the cell at coordinate [c] on the
 * board in [st].
 * raises: [InvalidPlace] if [c] does not correspond to a cell in the board. *)
let get_cell_from_coordinate c st =
  try
    let row = get_row c st in
    List.nth row (snd c)
  with
  | exn -> raise InvalidPlace

(* [cell_is_empty c] returns [true] if cell [c] is empty and returns [false] if
 * there is a [letter] at cell [c]. *)
let cell_is_empty c =
  c.letter = (' ', 0)

(* [get_horizontal_word c st] returns the horizontal word starting at coordinate
 * [c] on the board in [st]. If the cell at [c] is empty, the empty string is
 * returned. *)
let get_horizontal_word c st =
  let cell = get_cell_from_coordinate c st in
  let row = get_row c st in
  failwith ""

(* [get_vertical_word c st] returns the vertical word starting at coordinate
 * [c] on the board in [st]. If the cell at [c] is empty, the empty string is
 * returned. *)
let get_vertical_word c st =
  failwith ""

(* [place w c is_h] places word segment [w] at coordinate [c] horizontally if
 * [is_h] is true and vertically if [is_h] is false.
 * raises: [InvalidPlace] if one cannot place [w] at coordinate [c]. *)
let place w c is_h =
  failwith ""

(* [swap lst st] removes the letters in [lst] from the current player's rack and
 * swaps them with letters in the bag.
 * returns: the updated [state] after the swap.
 * raises: [InvalidSwap] if a letter in [lst] is not in the current
 * player's rack or there are not enough letters in the bag to do the swap. *)
let swap lst st =
  let player = st.current_player in
  let valid_swap = List.for_all (fun (x) -> List.mem_assoc x player.rack) lst in (*david*)
    (* List.fold_left (fun acc l -> acc && List.mem_assoc l player.rack) true lst in *)
  if (List.length st.bag >= List.length lst) && valid_swap then
    let remove_chars_rack =
      List.fold_left (fun acc c -> List.remove_assoc c acc) player.rack lst in
    let st' = bag_to_rack remove_chars_rack st.bag player st in
    let updated_current_player =
      List.hd (List.filter (fun p -> p.name = player.name) st'.players) in
    let rec add_to_bag l b =
      match l with
      | [] -> b
      | h::t -> add_to_bag t ((h, get_points h) :: b)
    in let updated_bag = add_to_bag lst st'.bag in
    {st' with bag = updated_bag; current_player = updated_current_player}
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
