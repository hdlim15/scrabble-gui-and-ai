open Trie
open Command

type coordinate = int * int

type letter = (char * int)

type cell = {
  cell_coord : coordinate;
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
  order_num : int;
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

let update_rack_and_bag chars_from_rack rack bag =
  let rack' =
    List.fold_left (fun acc c -> List.remove_assoc c acc) rack chars_from_rack in
  let rec update_rack r b =
    if List.length r = 7 then
      (r, b)
    else
      begin
        Random.self_init ();
        let i = Random.int (List.length b) in
        let letter_from_bag = List.nth b i in
        let updated_bag = List.remove_assoc (fst letter_from_bag) bag in
        update_rack (letter_from_bag :: r) updated_bag
      end
  in
  update_rack rack' bag

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
  {cell_coord = row_num, col_num;
   letter = (' ', -1);
   letter_multiplier = 1;
   word_multiplier = 1}

(* [init_bag ()] creates a scrabble bag of tiles *)
let rec init_bag () =
  let alphabet = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';
                  'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z';'*'] in
  List.flatten (List.map init_letters_of_char alphabet)
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
  | i, name::t -> {name=name;score=0;rack=[];player_type=Human;order_num=i}
                  :: gen_human_players (num-1) t
  | _ -> failwith "impossible, num = names.length"

let rec gen_ai_players num_players num_ai difficulty =
  match num_ai, difficulty with
  | 0, [] -> []
  | i, diff::t -> {name="AI_"^(string_of_int i);score=0;rack=[];player_type=AI diff;order_num=num_players-i+1}
                  :: gen_ai_players num_players (num_ai-1) t
  | _ -> failwith "impossible, num = difficulty.length"

let init_players init_data =
  let num_players = init_data.num_players in
  let num_humans = init_data.num_humans in
  if num_players = num_humans then
    gen_human_players num_players init_data.human_names
  else
    let human_players = gen_human_players num_humans init_data.human_names in
    let ai_players =
      gen_ai_players num_players (num_players-num_humans) init_data.ai_difficulty in
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

(* dictionary *)
let dict = Trie.initialize_dict "forward_dict.txt"

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
  c.letter = (' ', -1)

(* [fst_triple t] returns the first element from tuple [t] with three
 * elements. *)
let fst_triple t =
  match t with
  | (x, _, _) -> x

(* [snd_triple t] returns the second element from tuple [t] with three
 * elements. *)
let snd_triple t =
  match t with
  | (_, x, _) -> x

(* [trd_triple t] returns the third element from tuple [t] with three
 * elements. *)
let trd_triple t =
  match t with
  | (_, _, x) -> x

(* [get_adjacent_cells c st is_h] returns a list of cells that contains the
 * adjacent word formed by placing a letter at coordinate [c]. [is_h] determines
 * whether the adjacent word is searched for horizontally or vertically. *)
let get_adjacent_cells c st is_h =
  let cell = get_cell_from_coordinate c st in
  let cell_list =
    if is_h then
      get_row c st
    else
      get_column c st
  in
  let rec start_coord_helper lst acc =
    match lst with
    | [] -> acc
    | h::t ->
      begin
        match h.letter with
        | (' ', _) ->
          if List.mem cell acc then
            acc
          else
            start_coord_helper t []
        | (x, _) -> start_coord_helper t (h :: acc)
      end
  in start_coord_helper cell_list []

(* [get_adjacent_word c st is_h] returns a pair with the adjacent word at
 * coordinate [c] on the board in [st] and the points associated with it. [is_h]
 * determines whether the adjacent word is searched for horizontally or
 * vertically. If the cell at [c] is empty, the empty string is returned with 0
 * points. *)
let get_adjacent_word c st is_h =
  let word_cells = get_adjacent_cells c st is_h in
  let rec adjacent_helper lst (acc : (string * int * int)) =
    match lst with
    | [] -> acc
    | h::t ->
      let new_string = (fst_triple acc) ^ (Char.escaped (fst h.letter)) in
      let points = (snd_triple acc) + (h.letter_multiplier * (snd h.letter)) in
      let word_multiplier = (trd_triple acc) * h.word_multiplier in
      adjacent_helper t (new_string, points, word_multiplier)
    in
    let word_triple = adjacent_helper word_cells ("", 0, 1) in
    (fst_triple word_triple, (snd_triple word_triple) * (trd_triple word_triple))

(* [get_next_player n p] returns the player whose turn is next given the
 * current player's order number [n] and a list of the players [p]. *)
let rec get_next_player n p =
  let n' =
    if n = List.length p then 0
    else n
  in
  match p with
  | [] -> failwith "Invalid n value"
  | h::t ->
    if h.order_num = n' + 1 then h
    else get_next_player n t

let place_horizontal mv st =
  let row_idx = fst mv.mv_coord in
  let rec helper board count =
    match board with
    | [] -> []
    | row::t ->
      if count <> row_idx then
        row :: helper t (count+1)
      else
        (* row corresponds to row to which we want to add chars *)
        let rec helper' row count =
          match row with
          | [] -> []
          | cell::t -> (* coord is (0,0) *)
            if count < (snd mv.mv_coord) ||
               count >= (snd mv.mv_coord + (List.length mv.word)) then
              cell :: helper' t (count+1)
            else (* cell letter needs to be updated *)
              let letter_char = List.nth mv.word (count - (snd mv.mv_coord)) in
              let letter_points = get_points letter_char in
              {cell with letter = (letter_char, letter_points)} :: helper' t (count+1)
        in helper' row 0 :: helper t (count+1)
  in helper st.board 0

let place_vertical mv st =
  failwith "todo"

let check_word word st =
  (Trie.is_word dict word) || (List.mem word st.added_words)

let check_bounds mv st =
  let word = List.fold_right (fun c acc -> (Char.escaped c)^acc) mv.word "" in
  if mv.is_horizontal then (snd mv.mv_coord) + String.length word < 15
  else (fst mv.mv_coord) + String.length word < 15

let check_endpoints mv st =
  if mv.is_horizontal then
    let left_empty =
      try
        get_cell_from_coordinate (fst mv.mv_coord, snd mv.mv_coord - 1) st |> cell_is_empty
      with
      | InvalidPlace -> true in
    let right_empty =
      try
        get_cell_from_coordinate (fst mv.mv_coord, snd mv.mv_coord + (List.length mv.word)) st |> cell_is_empty
      with
      | InvalidPlace -> true in
    left_empty && right_empty
  else
    let top_empty =
      try
        get_cell_from_coordinate (fst mv.mv_coord - 1, snd mv.mv_coord) st |> cell_is_empty
      with
      | InvalidPlace -> true in
    let bottom_empty =
      try
        get_cell_from_coordinate (fst mv.mv_coord + (List.length mv.word), snd mv.mv_coord) st |> cell_is_empty
      with
      | InvalidPlace -> true in
    top_empty && bottom_empty

let tentatively_valid_move mv st =
  let word = List.fold_right (fun c acc -> (Char.escaped c)^acc) mv.word "" in
  check_word word st && check_bounds mv st && check_endpoints mv st

(* [check_fit_and_new_entries mv st] is the list of new chars being placed on
 * the board, assuming mv.word doesn't violate the current board state.
 * raises: InvalidPlace when mv.word violates current board state. *)
let check_fit_and_new_entries mv st =
  if mv.is_horizontal then
    let row = get_row mv.mv_coord st in
    let rec helper row count acc =
      match row with
      | [] -> acc
      | cell::t ->
        if count < (snd mv.mv_coord) ||
           count >= (snd mv.mv_coord + (List.length mv.word)) then
          helper t (count+1) acc
        else (* cell contains piece of new word *)
        if fst cell.letter = (List.nth mv.word (count - (snd mv.mv_coord))) then
          (* pre-existing letter, not coming from player rack *)
          helper t (count+1) acc
        else
        if fst cell.letter <> ' ' then raise InvalidPlace
        else (* adding char from rack to this cell *)
          helper t (count+1)
            ((List.nth mv.word (count - (snd mv.mv_coord)), (fst mv.mv_coord, count)) :: acc)
    in helper row 0 []
  else
    let col = get_column mv.mv_coord st in
    let rec helper col count acc =
      match col with
      | [] -> acc
      | cell::t ->
        if count < (fst mv.mv_coord) ||
           count >= (fst mv.mv_coord + (List.length mv.word)) then
          helper t (count+1) acc
        else (* cell contains piece of new word *)
        if fst cell.letter = (List.nth mv.word (count - (fst mv.mv_coord))) then
          (* pre-existing letter, not coming from player rack *)
          helper t (count+1) acc
        else
        if fst cell.letter <> ' ' then raise InvalidPlace
        else (* adding char from rack to this cell *)
          helper t (count+1)
            ((List.nth mv.word (count - (snd mv.mv_coord)), (fst mv.mv_coord, count)) :: acc)
    in helper col 0 []

let rec remove c lst =
  match lst with
  | [] -> []
  | h::t -> if h = c then remove c t
    else h :: remove c t

(* [check_rack rack new_board_chars] checks that new_board_chars is a subset of rack *)
let check_rack rack new_board_chars =
  let board_chars = List.map (fun (c, coord) -> c) new_board_chars in
  let rack' = List.map (fun (c,p) -> c) rack in
  if List.length rack < List.length board_chars then false
  else
    let rec helper r new_board_chars =
      match new_board_chars with
      | [] -> true
      | c::t ->
        if List.mem c r && List.mem c board_chars then
          true && helper (remove c r) t
        else false
    in helper rack' board_chars

let update_board mv st =
  if mv.is_horizontal then
    place_horizontal mv st
  else place_vertical mv st

let update_players current_player rack players new_points =
  let updated_player =
    {current_player with rack = rack; score = current_player.score + new_points} in
  updated_player ::
  (List.filter (fun p -> p.name <> updated_player.name) players)

(* [place w c is_h] places word segment [w] at coordinate [c] horizontally if
 * [is_h] is true and vertically if [is_h] is false.
 * raises: [InvalidPlace] if one cannot place [w] at coordinate [c]. *)
let rec place mv st =

  if not (tentatively_valid_move mv st) then raise InvalidPlace
  else
    (* check first-move-of-game condition *)
    if List.for_all (fun p -> p.score = 0) st.players then
      let board' = update_board mv st in
      let row7 = List.nth board' 7 in
      let cell7 = List.nth row7 7 in
      if cell_is_empty cell7 then
        raise InvalidPlace
      else (* word was placed on center tile *)
        (* update score, player rack, current player, bag *)
        let current_player = st.current_player in
        let rack_bag = update_rack_and_bag mv.word current_player.rack st.bag in
        let word_score =
          get_adjacent_word mv.mv_coord {st with board = board'} mv.is_horizontal in
        let updated_players =
          update_players current_player (fst rack_bag) st.players (snd word_score) in
        let next_player = get_next_player current_player.order_num updated_players in
        {st with players = updated_players;
                 board = board';
                 current_player = next_player;
                 bag = (snd rack_bag)}
    else (* not first move of game *)
      (* new_chars is an assoc list of character*coord *)
      let new_chars = check_fit_and_new_entries mv st in
      (* if lengths are equal, move invalid because no pre-existing letters on board *)
      if List.length new_chars = List.length mv.word then raise InvalidPlace
      else
      if not (check_rack st.current_player.rack new_chars) then
        raise InvalidPlace (* newly-placed chars not in player rack *)
      else
        (* assuming place is valid... *)
        let board' = update_board mv st in
        let just_new_chars = List.map (fun (c,coord) -> c) new_chars in
        let current_player = st.current_player in
        let rack_bag = update_rack_and_bag just_new_chars current_player.rack st.bag in
        let updated_current_player = get_next_player current_player.order_num st.players in
        let updated_players = update_players current_player (fst rack_bag) st.players in
        (* update score, change turn, update player rack and bag etc *)
        {st with board = board'}

(* [swap lst st] removes the letters in [lst] from the current player's rack and
 * swaps them with letters in the bag.
 * returns: the updated [state] after the swap.
 * raises: [InvalidSwap] if a letter in [lst] is not in the current
 * player's rack or there are not enough letters in the bag to do the swap. *)
let swap lst st =
  let player = st.current_player in
  let valid_swap = List.for_all (fun (x) -> List.mem_assoc x player.rack) lst in
  if (List.length st.bag >= List.length lst) && valid_swap then
    let rack_bag = update_rack_and_bag lst player.rack st.bag in
    let players' = update_players player (fst rack_bag) st.players 0 in
    let st' = {st with players = players'; bag = (snd rack_bag)} in
    let updated_current_player = get_next_player player.order_num st'.players in
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
  | PlaceWord mv -> place mv st
  | _ -> st
