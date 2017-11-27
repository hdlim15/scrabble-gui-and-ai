open Command
open Trie
open State

type direction = Left | Right | Up | Down

let f_dict = Trie.initialize_dict "forward_dict.txt"

let r_dict = Trie.initialize_dict "reverse_dict.txt"

let vowels = ['a';'e';'i';'o';'u']

let extends_forward str =
  List.length (get_extensions str f_dict) <> 0

let extends_reverse str =
  List.length (get_extensions str r_dict) <> 0

(* [get_all_cells st] returns a list of all cells currently on the board in
 * state [st]
 *)
let get_all_cells st =
  List.flatten st.board

(* [get_empty_cells lst] returns a list of all empty cells in [lst]. *)
let get_empty_cells lst =
  List.filter (fun c -> (cell_is_empty c)) lst

(* [get_7th_row_cells st returns a list of all cells in the row with index 7. *)
let get_7th_row_cells st =
  get_row (7,7) st

let reverse_str s =
  List.fold_right (fun x acc -> acc ^ Char.escaped x ) (explode s) ""

let up_cell c =
  if fst (c.cell_coord ) = 0 then None
  else Some((fst (c.cell_coord ) - 1), snd (c.cell_coord ))

let down_cell c =
  if fst (c.cell_coord ) = 14 then None
  else Some((fst (c.cell_coord ) + 1), snd (c.cell_coord ))

let left_cell c =
  if snd (c.cell_coord ) = 0 then None
  else Some((fst (c.cell_coord )), (snd (c.cell_coord )) - 1)

let right_cell c =
  if snd (c.cell_coord ) = 14 then None
  else Some((fst (c.cell_coord )), (snd (c.cell_coord )) + 1)

(* [adjacent_coordinates c] returns [up;down;left;right], where
 * [up] is [up_cell c], [down] is [down_cell c],
 * [left] is [left_cell c] and [right] is [right_cell c]
 *)
let adjacent_coordinates c =
  let up = up_cell c in
  let down = down_cell c in
  let left = left_cell c in
  let right = right_cell c in
  [left;right;up;down]

(* [has_adjacent_word_tile c st] returns [true] if [c] has a non_empty
 * adjacent tile in state [st], and [false] otherwise.
 *)
let has_adjacent_word_tile c st =
  List.fold_left
    (fun acc x ->
       match x with
       | None -> acc
       | Some t ->
         acc || (not(get_cell_from_coordinate t st |> cell_is_empty)))
    false (adjacent_coordinates c)

(* [get_anchors empty_cells st] returns a list of all anchors currently on the
 * board in state [st], where an anchor is an empty tile
 * having at least one non-empty adjacent tile.
 *)
let get_anchors empty_cells st =
  List.filter (fun c -> has_adjacent_word_tile c st ) empty_cells

let get_all_adj_words c st =
  let left =
    match left_cell c with
    | None -> ""
    | Some c' ->
      match get_adjacent_word c' st true [] with
      | None ->
        let cell' = get_cell_from_coordinate c' st in
        if cell_is_empty cell' then ""
        else fst (cell'.letter) |> Char.escaped
      | Some (word,_,_) -> word in
  let right =
    match right_cell c with
    | None -> ""
    | Some c' ->
      match get_adjacent_word c' st true [] with
      | None ->
        let cell' = get_cell_from_coordinate c' st in
        if cell_is_empty cell' then ""
        else fst (cell'.letter) |> Char.escaped
      | Some (word,_,_) -> word in
  let up =
    match up_cell c with
    | None -> ""
    | Some c' ->
      match get_adjacent_word c' st false [] with
      | None ->
        let cell' = get_cell_from_coordinate c' st in
        if cell_is_empty cell' then ""
        else fst (cell'.letter) |> Char.escaped
      | Some (word,_,_) -> word in
  let down =
    match down_cell c with
    | None -> ""
    | Some c' ->
      match get_adjacent_word c' st false [] with
      | None ->
        let cell' = get_cell_from_coordinate c' st in
        if cell_is_empty cell' then ""
        else fst (cell'.letter) |> Char.escaped
      | Some (word,_,_) -> word in
  [left;right;up;down]

(* let rec get_next_word c st dir =
  match dir with
  | Right ->
    begin
      match right_cell c with
      | None -> c.cell_coord,""
      | Some c' ->
        let new_cell = get_cell_from_coordinate c' st in
        if (new_cell |> cell_is_empty) then get_next_word new_cell st Right
        else
          match get_adjacent_word c' st true [] with
          | None -> c', fst (new_cell.letter) |> Char.escaped
          | Some (word,_,_) -> c', word
    end
  | Left ->
    begin
      match left_cell c with
      | None -> c.cell_coord,""
      | Some c' ->
        let new_cell = get_cell_from_coordinate c' st in
        if (new_cell |> cell_is_empty) then get_next_word new_cell st Left
        else
          match get_adjacent_word c' st true [] with
          | None -> c',fst (new_cell.letter) |> Char.escaped
          | Some (word,_,_) -> c', word
    end
  | Up ->
    begin
      match up_cell c with
      | None -> c.cell_coord,""
      | Some c' ->
        let new_cell = get_cell_from_coordinate c' st in
        if (new_cell |> cell_is_empty) then get_next_word new_cell st Up
        else
          match get_adjacent_word c' st false [] with
          | None -> c',fst (new_cell.letter) |> Char.escaped
          | Some (word,_,_) -> c',word
    end
  | Down ->
    begin
      match down_cell c with
      | None -> c.cell_coord,""
      | Some c' ->
        let new_cell = get_cell_from_coordinate c' st in
        if (new_cell |> cell_is_empty) then get_next_word new_cell st Down
        else
          match get_adjacent_word c' st false [] with
          | None -> c',fst (new_cell.letter) |> Char.escaped
          | Some (word,_,_) -> c',word
    end *)

let cross_check_helper cell st is_h =
  match get_adjacent_word cell st is_h [] with
  | None ->
    let cell' = get_cell_from_coordinate cell st  in
    if not (cell' |> cell_is_empty) then (fst cell'.letter) |> Char.escaped
    else ""
  | Some (word,_,_) -> word

(* [cross_check c chr st] returns true if, when placed on tile [c],
 * [chr] forms valid forms with the adjacent tiles on the board in state [st].
 *)
let cross_check c chr st =
  let left =
    match left_cell c with
    | None -> ""
    | Some c' -> cross_check_helper c' st true in
  let right =
    match right_cell c with
    | None -> ""
    | Some c' -> cross_check_helper c' st true in
  let up =
    match up_cell c with
    | None -> ""
    | Some c' -> cross_check_helper c' st false in
  let down =
    match down_cell c with
    | None -> ""
    | Some c' -> cross_check_helper c' st false in
  let bool1 =
    let hor = left ^ (Char.escaped chr) ^ right in
    if hor = (Char.escaped chr) then true
    else
      (is_word f_dict hor) || extends_forward hor ||
      extends_reverse (reverse_str hor) in
  let bool2 =
    let vert = up ^ (Char.escaped chr) ^ down in
    if vert = (Char.escaped chr) then true
    else
      (is_word f_dict vert) || extends_forward vert ||
      extends_reverse (reverse_str vert) in
  bool1 && bool2

(* [anchor_chars anchor rack st] returns the new rack obtained after
 * throwing out all characters from [rack] that don't form valid words
 * with the surrounding tiles when placed on the anchor cell [anchor].
 *)
let anchor_chars anchor rack st =
  List.fold_left
    (fun acc x ->
       if cross_check anchor x st then x::acc else acc
    ) [] rack

(* [generate_anchor_chars anchors rack st ] returns a list of pairs
 * in which the first item in pair is an anchor cell,
 * and the second item is that anchor's corresponding rack after
 * calculating the new rack obtained after throwing out all characters from
 * [rack] that don't form valid words with the surrounding tiles when placed on
 * that anchor cell.
 *)
let generate_anchor_chars anchors rack st =
  List.fold_left
    (fun acc x ->
       (x, anchor_chars x rack st )::acc
    ) [] anchors

(* [check_extension anchor_rack ext] returns [true] if [ext] can be formed
 * by some permutation of the characters in [anchor_rack], and [false] otherwise.
 *)
let check_extension anchor_rack ext =
  let check =
  List.fold_left
    (fun (check, rack') x ->
       if not check then false, rack'
       else
         if List.mem x rack' then (true, remove x rack')
         else if List.mem '*' rack' then (true, remove '*' rack')
         else false, rack'
    ) (true, anchor_rack) (explode ext) in
  fst check

(* [valid_extensions anchor_rack extension_lst] returns a list of all
 * extensions in [extension_lst] that can be formed by some permutation of the
 * characters in [anchor_rack].
 *)
let valid_extensions anchor_rack extension_lst =
  List.filter (fun x -> check_extension anchor_rack x) extension_lst

(* [concat_moves str exts] concatanates each element of [exts] to [str]
 * and returns the updated list.
 *)
let concat_moves str exts =
  List.fold_left
    (fun acc x -> (str ^ x)::acc ) [] exts

(* [concat_moves str exts] concatanates [str\ to each element of [exts]
 * and returns the updated list.
 *)
let concat_moves_rev str exts =
  List.fold_left
    (fun acc x -> ((reverse_str x) ^ str)::acc ) [] exts

(* [cut_extensions lst] removes all strings of length more than 6 from [lst].
 * Used to generate the first move of the game.
 *)
let cut_extensions lst =
  List.filter (fun x -> String.length x < 7) lst

let move_forward cell rack st is_h across_bool =
  if not across_bool then
  match get_adjacent_word cell st is_h [] with
  | None -> None
  | Some (str,_,_) ->
  let extensions = get_extensions str f_dict in
  let words = (valid_extensions rack extensions) |> concat_moves str in
  Some (words,str)
  else
    if (get_cell_from_coordinate cell st |> cell_is_empty) then None
    else
      let letter = fst ((get_cell_from_coordinate cell st).letter) in
      let across_ext = get_extensions (Char.escaped letter) f_dict in
      let words = (valid_extensions rack across_ext)
                  |> concat_moves(Char.escaped letter) in
      Some (words,(Char.escaped letter))

let move_backward cell rack st is_h across_bool =
  if not across_bool then
    match get_adjacent_word cell st is_h [] with
  | None -> None
  | Some (str,_,__) ->
    let extensions = get_extensions (reverse_str str) r_dict in
    let words = (valid_extensions rack extensions) in
    Some (words,str)
  else
    if (get_cell_from_coordinate cell st |> cell_is_empty) then None
    else
      let letter = fst ((get_cell_from_coordinate cell st).letter) in
      let across_ext = get_extensions (Char.escaped letter) r_dict in
    let words = (valid_extensions rack across_ext) in
      Some (words, Char.escaped letter)

let make_move c rack st =
  let left =
  match left_cell c with
  | None -> None
  | Some c' -> move_forward c' rack st true false in
  let left_across =
    match left_cell c with
    | None -> None
    | Some c' -> move_forward c' rack st true true in
  let right =
  match right_cell c with
  | None -> None
  | Some c' ->  move_backward c' rack st true false in
  let right_across =
    match right_cell c with
    | None -> None
    | Some c' -> move_backward c' rack st true false in
  let up =
  match up_cell c with
  | None -> None
  | Some c' -> move_forward c' rack st false false in
  let up_across =
    match up_cell c with
    | None -> None
    | Some c' -> move_forward c' rack st false true in
  let down =
    match down_cell c with
    | None -> None
    | Some c' -> move_backward c' rack st false false in
  let down_across =
      match down_cell c with
      | None -> None
      | Some c' -> move_backward c' rack st false true in
  [left;right;up;down;left_across;right_across;up_across;down_across]

let all_moves anchors st =
  List.fold_left
    (fun acc x ->
       (fst x,(make_move (fst x) (snd x) st))::acc
    ) [] anchors

let start_forward cell rack st =
  if (get_cell_from_coordinate cell st |> cell_is_empty) then None
  else Some(
      List.fold_left
        (fun acc x ->
           let chr = (Char.escaped x) in
           let extensions = get_extensions chr f_dict |> cut_extensions in
           let words = (valid_extensions (remove x rack) extensions)
                       |> concat_moves chr in
           (words,chr)::acc
        ) [] (List.sort_uniq compare rack))

let start_backward cell rack st =
  if (get_cell_from_coordinate cell st |> cell_is_empty) then None
  else Some(
      List.fold_left
        (fun acc x ->
           let chr = (Char.escaped x) in
           let extensions = get_extensions chr r_dict |> cut_extensions in
           let words = (valid_extensions (remove x rack) extensions) in
           (words,chr)::acc
        ) [] (List.sort_uniq compare rack))

let make_more_moves c rack st =
  let left_down =
    match left_cell c with
    | None -> None
    | Some c' -> start_forward c' rack st in
  let left_up =
    match left_cell c with
    | None -> None
    | Some c' -> start_backward c' rack st in
  let right_down =
    match right_cell c with
    | None -> None
    | Some c' -> start_forward c' rack st in
  let right_up =
    match right_cell c with
    | None -> None
    | Some c' -> start_backward c' rack st in
  let up_right =
    match up_cell c with
    | None -> None
    | Some c' -> start_forward c' rack st in
  let up_left =
    match up_cell c with
    | None -> None
    | Some c' -> start_backward c' rack st in
  let down_right =
    match down_cell c with
    | None -> None
    | Some c' -> start_forward c' rack st in
  let down_left =
    match down_cell c with
    | None -> None
    | Some c' -> start_backward c' rack st in
  [left_down;left_up;right_down;right_up;up_right;up_left;down_right;down_left]

let all_more_moves anchors st =
  List.fold_left
    (fun acc x ->
       (fst x,(make_more_moves (fst x) (snd x) st))::acc
    ) [] anchors

let make_first_move chr rack st =
  let str = Char.escaped chr in
  let left =
    let extensions = get_extensions str f_dict in
    let words = (valid_extensions rack extensions) |> concat_moves str in
    (words,str)in
  let right =
    let extensions = get_extensions (reverse_str str) r_dict in
    let words = (valid_extensions rack extensions) in
    (words,str) in
  [left;right]

let all_first_moves anchor rack st =
  List.fold_left
    (fun acc x ->
       (anchor, (make_first_move x (remove x rack) st))::acc
    ) [] rack

let get_start_cell anchor word dir =
  match dir with
  | Left ->
    let subtract = String.length (word) in
    ((fst (anchor.cell_coord )), (snd (anchor.cell_coord )) - subtract)
  | Up ->
    let subtract = String.length (word) in
    ((fst (anchor.cell_coord ) - subtract), snd (anchor.cell_coord ))
  | Right ->
    let subtract = String.length (word) - 1 in
    ((fst (anchor.cell_coord )), (snd (anchor.cell_coord )) - subtract)
  | Down ->
    let subtract = String.length (word) - 1  in
    ((fst (anchor.cell_coord ) - subtract), snd (anchor.cell_coord ))

let  get_all_first_move_start_cells anchor word_lst st =
  let left =
    let pair = List.nth word_lst 0 in
      (List.fold_left
        (fun acc x ->
           let updated_cell = (get_start_cell anchor (snd pair) Left) in
           (updated_cell, x)::acc
        ) [] (fst pair)) in
  let right =
    let pair = List.nth word_lst 1 in
      List.fold_left
        (fun acc x ->
           let updated_cell = (get_start_cell anchor x Right) in
           let new_word = (reverse_str x ^ (snd pair)) in
           (updated_cell, new_word)::acc
        ) [] (fst pair) in
  [left;right]

let update_all_first_move_anchor_pairs anchor_pair_lst st =
  List.map (fun x -> get_all_first_move_start_cells
               (fst x) (snd x) st) anchor_pair_lst

let get_forward_start_cell anchor str exts dir =
  (List.fold_left
    (fun acc x ->
       let updated_cell = (get_start_cell anchor (str) dir) in
       (updated_cell, x)::acc
    ) [] (exts))

let get_backward_start_cell anchor str exts dir =
  List.fold_left
    (fun acc x ->
       let updated_cell = (get_start_cell anchor x dir) in
       let new_word = (reverse_str x ^ (str)) in
       (updated_cell, new_word)::acc
    ) [] (exts)

let get_all_start_cells anchor word_lst st =
  let left =
    match List.nth word_lst 0 with
    | None -> []
    | Some pair -> get_forward_start_cell anchor (snd pair) (fst pair) Left in
  let right =
    match List.nth word_lst 1 with
    | None -> []
    | Some pair -> get_backward_start_cell anchor (snd pair) (fst pair) Right in
  let up =
    match List.nth word_lst 2 with
    | None -> []
    | Some pair -> get_forward_start_cell anchor (snd pair) (fst pair) Up in
  let down =
    match List.nth word_lst 3 with
    | None -> []
    | Some pair -> get_backward_start_cell anchor (snd pair) (fst pair) Down in
  let left_across =
    match List.nth word_lst 4 with
    | None -> []
    | Some pair -> get_forward_start_cell anchor (snd pair) (fst pair) Left in
  let right_across =
    match List.nth word_lst 5 with
    | None -> []
    | Some pair -> get_backward_start_cell anchor (snd pair) (fst pair) Right in
  let up_across =
    match List.nth word_lst 6 with
    | None -> []
    | Some pair -> get_forward_start_cell anchor (snd pair) (fst pair) Up in
  let down_across =
    match List.nth word_lst 7 with
    | None -> []
    | Some pair -> get_backward_start_cell anchor (snd pair) (fst pair) Down in
  [left @ right @ left_across @ right_across;
   up @ down @ up_across @ down_across]

let update_all_anchor_pairs anchor_pair_lst st =
  List.map (fun x -> get_all_start_cells (fst x) (snd x) st) anchor_pair_lst

let more_start_cells_forward anchor lst =
  (List.fold_left
     (fun accu elm ->
        (List.fold_left
           (fun acc x ->
              let updated_cell =
                (fst anchor.cell_coord, snd anchor.cell_coord) in
              (updated_cell, x) :: acc
           ) [] (fst elm))::accu
     ) [] lst) |> List.flatten

let more_start_cells_backward anchor lst dir =
  (List.fold_left
     (fun accu elm ->
        (List.fold_left
           (fun acc x ->
              let updated_cell = (get_start_cell anchor (x) dir) in
              let new_word = (reverse_str x ^ (snd elm)) in
              (updated_cell, new_word ) :: acc
           ) [] (fst elm))::accu
     ) [] lst) |> List.flatten

let get_all_more_start_cells anchor word_lst st =
  let left_down =
    match List.nth word_lst 0 with
    | None -> []
    | Some lst -> more_start_cells_forward anchor lst in
  let left_up =
    match List.nth word_lst 1 with
    | None -> []
    | Some lst -> more_start_cells_backward anchor lst Up in
  let right_down =
    match List.nth word_lst 2 with
    | None -> []
    | Some lst -> more_start_cells_forward anchor lst in
  let right_up =
    match List.nth word_lst 3 with
    | None -> []
    | Some lst -> more_start_cells_backward anchor lst Up in
  let up_right =
    match List.nth word_lst 4 with
    | None -> []
    | Some lst -> more_start_cells_forward anchor lst in
  let up_left =
    match List.nth word_lst 5 with
    | None -> []
    | Some lst -> more_start_cells_backward anchor lst Left in
  let down_right =
    match List.nth word_lst 6 with
    | None -> []
    | Some lst -> more_start_cells_forward anchor lst in
  let down_left =
    match List.nth word_lst 7 with
    | None -> []
    | Some lst -> more_start_cells_backward anchor lst Left in
  [up_left @ up_right @ down_right @ down_left;
   left_up @ left_down @ right_up @ right_down]

let update_all_more_anchor_pairs anchor_pair_lst st =
  List.map (fun x ->
      get_all_more_start_cells (fst x) (snd x) st) anchor_pair_lst

let get_points mv st =
  let word = List.fold_right (fun c acc -> (Char.escaped c)^acc) mv.word "" in
  if not (is_word f_dict word) then raise (InvalidPlace "invalid word")
  else if not (check_bounds mv st)
  then raise (InvalidPlace "cannot place off board")
  else if not (check_endpoints mv st)
  then raise (InvalidPlace "not complete word")
  else
    (* new_chars is an assoc list of character*coord *)
    let new_chars = check_fit_and_new_entries mv st in
    (* assuming place is valid... *)
    let board' = update_board mv st in
    let new_coords = List.map (fun (_,coord) -> coord) new_chars in
    let st_board = {st with board = board'} in
    if List.length new_chars = List.length mv.word &&
       not (List.fold_left
              (fun acc (_, c) ->
                 (has_adj_new_chars c (not mv.is_horizontal) st_board)
                 || acc) false new_chars) then
      raise (InvalidPlace "not connected to board")
    else
      let word_score_opp_dir_opt =
        List.fold_left (fun acc c ->
            (get_adjacent_word (snd c) st_board
               (not mv.is_horizontal) new_coords) :: acc) [] new_chars in
      let word_score_lst_opt =
        (get_adjacent_word mv.mv_coord st_board mv.is_horizontal new_coords)
        :: word_score_opp_dir_opt in
      let word_score_lst = get_values_from_opt_list word_score_lst_opt [] in
      let valid_words =
        List.fold_left (fun acc (s, i, _) ->
            (fst acc  && check_word s st, snd acc + i)) (true, 0) word_score_lst
      in
      let score' =
        if List.length new_chars = 7 then (snd valid_words + 50) else (snd valid_words) in
      if fst valid_words then score'
      else
        raise (InvalidPlace "invalid newly-formed word")

let get_first_move_points mv st =
    let board' = update_board mv st in
    let row7 = List.nth board' 7 in
    let cell7 = List.nth row7 7 in
    if cell_is_empty cell7 then
      raise (InvalidPlace "must fill center tile")
    else (* word was placed on center tile *)
      (* update score, player rack, current player, bag *)
      let new_chars = check_fit_and_new_entries mv st in
      let new_coords = List.map (fun (_,coord) -> coord) new_chars in
      let word_score_opt =
        get_adjacent_word mv.mv_coord {st with board = board'}
          mv.is_horizontal new_coords in
      let word_score = List.hd (get_values_from_opt_list [word_score_opt] []) in
      let score' =
        if List.length new_chars = 7 then (snd_triple word_score + 50)
        else (snd_triple word_score) in
      score'

let generate_move cell str dir =
  match dir with
  | Left | Right -> {word = explode str; mv_coord = cell; is_horizontal = true;}
  | Up | Down -> {word = explode str; mv_coord = cell; is_horizontal = false;}

let generate_moves_for_anchor move_lst =
  let left_right =
    List.fold_left
      (fun acc x ->
         (generate_move (fst x) (snd x) Left)::acc
      ) [] (List.nth move_lst 0) in
  let up_down =
    List.fold_left
      (fun acc x ->
         (generate_move (fst x) (snd x) Up)::acc
      ) [] (List.nth move_lst 1) in
  left_right @ up_down

let generate_all_moves all_moves =
  List.fold_left
    (fun acc x -> (generate_moves_for_anchor x)::acc) [] all_moves
  |> List.flatten

let evaluate_rack rack =
  List.fold_left
    (fun acc x ->
       if List.mem x vowels then (fst acc + 1, snd acc)
       else (fst acc, snd acc + 1)
    ) (0,0) rack

let do_swap rack st =
  if List.length (st.bag) <> 0 then Swap [List.hd rack]
  else Pass

let pick_best_move rack st moves =
  match moves with
  | [] -> do_swap rack st
  | _ ->
  let best_move =
    List.fold_left (
      fun acc x ->
        try
          let new_points = get_points x st in
          if new_points > snd acc then x, new_points else acc
        with
        _ -> acc
    ) ({word = [];mv_coord = (0,0);is_horizontal = false}, -1) moves in
  if ( (snd best_move) = -1) then do_swap rack st
  else PlaceWord (fst best_move)

let pick_worst_move rack st moves =
  match moves with
  | [] -> do_swap rack st
  | _ ->
    let best_move =
      List.fold_left (
        fun acc x ->
          try
            let new_points = get_points x st in
            if new_points < snd acc then x, new_points else acc
          with
            _ -> acc
      ) ({word = [];mv_coord = (0,0);is_horizontal = false}, 100000) moves in
    if ( (snd best_move)= 100000) then do_swap rack st
    else PlaceWord (fst best_move)

let best_first_move moves rack st =
  match moves with
  | [] -> do_swap rack st
  | _ ->
    let best_move =
      List.fold_left (
        fun acc x ->
          try
            let new_points = get_first_move_points x st in
            if new_points > snd acc then x, new_points else acc
          with
            _ -> acc
      ) ({word = [];mv_coord = (0,0);is_horizontal = false}, -1000) moves in
    if ( (snd best_move) = -1000) then do_swap rack st
    else PlaceWord (fst best_move)

let get_letters_rack rack =
  List.map(fun (letter,_) -> letter) rack

let first_move st =
  let letters_rack = st.current_player.rack |> get_letters_rack in
  let anchor = get_cell_from_coordinate (7,8) st in
  let anchor_moves = all_first_moves anchor letters_rack st in
  let updated_anchors = update_all_first_move_anchor_pairs anchor_moves st in
  let moves = generate_all_moves updated_anchors in
  best_first_move moves letters_rack st

let best_move_helper st =
  let letters_rack = st.current_player.rack |> get_letters_rack in
  let all_cells = get_all_cells st in
  let empty_cells = get_empty_cells all_cells in
  let anchors = get_anchors empty_cells st in
  let anchor_pairs = generate_anchor_chars anchors letters_rack st in
  let anchor_moves = all_moves anchor_pairs st in
  let more_moves = all_more_moves anchor_pairs st in
  let updated_anchors = update_all_anchor_pairs anchor_moves st in
  let more_updates = update_all_more_anchor_pairs more_moves st in
  let moves = generate_all_moves updated_anchors in
  let new_moves = generate_all_moves more_updates in
  moves @ new_moves

let get_hint st =
  if List.for_all (fun p -> p.score = 0) st.players then first_move st
  else
    let letters_rack = st.current_player.rack |> get_letters_rack in
    best_move_helper st |> pick_worst_move letters_rack st

let best_move st =
  (* if List.for_all (fun p -> p.score = 0) st.players then first_move st *)
  if List.length st.bag = 86 then first_move st 
  else
    let letters_rack = st.current_player.rack |> get_letters_rack in
    best_move_helper st |> pick_best_move letters_rack st
