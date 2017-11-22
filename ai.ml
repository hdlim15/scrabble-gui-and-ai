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

let get_7th_row_cells st =
  get_row (7,7) st

let get_7th_column_cells st =
  get_column (7,7) st

(* [up_cell c] returns [Some c'] if [c'] is the cell coordinate
 * directly above [c], and [None] if the cell coordinate directly above [c]
 * is out of bounds.
 *)
let up_cell c =
  if fst (c.cell_coord ) = 0 then None
  else Some((fst (c.cell_coord ) - 1), snd (c.cell_coord ))

(* [down_cell c] returns [Some c'] if [c'] is the cell coordinate
 * directly below [c], and [None] if the cell coordinate directly below [c]
 * is out of bounds.
 *)
let down_cell c =
  if fst (c.cell_coord ) = 14 then None
  else Some((fst (c.cell_coord ) + 1), snd (c.cell_coord ))

(* [left_cell c] returns [Some c'] if [c'] is the cell coordinate
 * directly to the left of [c], and [None] if the cell coordinate
 * directly to the left of [c] is out of bounds.
 *)
let left_cell c =
  if snd (c.cell_coord ) = 0 then None
  else Some((fst (c.cell_coord )), (snd (c.cell_coord )) - 1)

(* [right_cell c] returns [Some c'] if [c'] is the cell coordinate
 * directly to the right of [c], and [None] if the cell coordinate
 * directly to the right of [c] is out of bounds.
 *)
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
 * board in state [st], where an anchor is an empty tile with
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
      | None -> ""
      | Some (word,_) -> word in
  let right =
    match right_cell c with
    | None -> ""
    | Some c' ->
      match get_adjacent_word c' st true [] with
      | None -> ""
      | Some (word,_) -> word in
  let up =
    match up_cell c with
    | None -> ""
    | Some c' ->
      match get_adjacent_word c' st false [] with
      | None -> ""
      | Some (word,_) -> word in
  let down =
    match up_cell c with
    | None -> ""
    | Some c' ->
      match get_adjacent_word c' st false [] with
      | None -> ""
      | Some (word,_) -> word in
  [left;right;up;down]

(* let cross_check c chr st =
  let left =
    match left_cell c with
    | None -> true
    | Some c' ->
      match get_adjacent_word c' st true [] with
      | None -> true
      | Some (word,_) ->
        let new_str = word ^ (Char.escaped chr) in
        (is_word f_dict new_str) || (extends_forward word)  in
  let right =
    match right_cell c with
    | None -> true
    | Some c' ->
      match get_adjacent_word c' st true [] with
      | None -> true
      | Some (word,_) ->
        let new_str = word ^ (Char.escaped chr) in
        (is_word f_dict new_str) || (extends_forward word)  in
  let up =
    match up_cell c with
    | None -> ""
    | Some c' ->
      match get_adjacent_word c' st false [] with
      | None -> ""
      | Some (word,_) -> word in
  let down =
    match up_cell c with
    | None -> ""
    | Some c' ->
      match get_adjacent_word c' st false [] with
      | None -> ""
      | Some (word,_) -> word in
  [left;right;up;down] *)


  (* let bool1 =
    match up_cell c with
    | None -> true
    | Some c' ->
      match get_adjacent_word c' st false [] with
      | None -> true
      | Some (str,_) ->
        is_word f_dict (str ^ (Char.escaped chr)) in
  let bool2 =
    match down_cell c with
    | None -> true
    | Some c' ->
      match get_adjacent_word c' st false [] with
      | None -> true
      | Some (str,_) ->
        is_word f_dict ((Char.escaped chr) ^ str) in
  bool1 || bool2 *)


let anchor_chars anchor rack st =
  List.fold_left
    (fun acc x ->
       x::acc
       (* if cross_check anchor x cells st then x::acc else acc *)
    ) [] rack

let generate_anchor_chars anchors rack st =
  List.fold_left
    (fun acc x ->
       (x, anchor_chars x rack st )::acc
    ) [] anchors

(* [check_extension anchor_rack ext] returns [true] if [ext] can be formed
 *by some permutation of the characters in [anchor_rack], and [false] otherwise.
 *)
let check_extension anchor_rack ext =
  let check =
  List.fold_left
    (fun (check, rack') x ->
       let check_bool = check && List.mem x rack' in
       if check_bool then check_bool, remove x rack'
       else false, rack'
    ) (true, anchor_rack) (explode ext) in
  fst check

(* [valid_extensions anchor_rack extension_lst] returns a list of all
 * extensions in [extension_lst] that can be formed by some permutation of the
 * characters in [anchor_rack].
 *)
let valid_extensions anchor_rack extension_lst =
  List.filter (fun x -> check_extension anchor_rack x) extension_lst
  (* List.fold_left
    (fun acc x ->
       let check = check_extension anchor_rack x in
       if fst check then x::acc else acc
    ) [] extensions *)

(* [reverse_str s ] reverses [s]. *)
let reverse_str s =
  List.fold_right (fun x acc -> acc ^ Char.escaped x ) (explode s) ""


let concat_moves str exts =
  List.fold_left
    (fun acc x -> (str ^ x)::acc ) [] exts

let concat_moves_rev str exts =
  List.fold_left
    (fun acc x -> ((reverse_str x) ^ str)::acc ) [] exts

let make_move c rack st =
  let left =
  match left_cell c with
  | None -> None
  | Some c' ->
      match get_adjacent_word c' st true [] with
    | None -> None
    | Some (str,_) ->
      let extensions = get_extensions str f_dict in
      let words = (valid_extensions rack extensions) |> concat_moves str in
      Some (words,str)in
  let left_across =
    match left_cell c with
    | None -> None
    | Some c' ->
      if (get_cell_from_coordinate c' st |> cell_is_empty) then None
      else
        let letter = fst ((get_cell_from_coordinate c' st).letter) in
        let across_ext = get_extensions (Char.escaped letter) f_dict in
        let words = (valid_extensions rack across_ext)
                    |> concat_moves(Char.escaped letter) in
        Some (words,(Char.escaped letter))in
  let right =
  match right_cell c with
  | None -> None
  | Some c' ->
      match get_adjacent_word c' st true [] with
    | None -> None
    | Some (str,_) ->
      let extensions = get_extensions (reverse_str str) r_dict in
      let words = (valid_extensions rack extensions) in
      Some (words,str) in
  let right_across =
    match right_cell c with
    | None -> None
    | Some c' ->
      if (get_cell_from_coordinate c' st |> cell_is_empty) then None
      else
        let letter = fst ((get_cell_from_coordinate c' st).letter) in
        let across_ext = get_extensions (Char.escaped letter) r_dict in
      let words = (valid_extensions rack across_ext) in
        Some (words, Char.escaped letter) in
  let up =
  match up_cell c with
  | None -> None
  | Some c' ->
      match get_adjacent_word c' st false [] with
    | None -> None
    | Some (str,_) ->
      let extensions = get_extensions str f_dict in
      let words = (valid_extensions rack extensions) |> concat_moves str in
      Some (words,str)in
  let up_across =
    match up_cell c with
    | None -> None
    | Some c' ->
      if (get_cell_from_coordinate c' st |> cell_is_empty) then None
      else
        let letter = fst ((get_cell_from_coordinate c' st).letter) in
        let across_ext = get_extensions (Char.escaped letter) f_dict in
        let words = (valid_extensions rack across_ext)
                    |> concat_moves(Char.escaped letter) in
        Some (words,(Char.escaped letter))in
  let down =
    match down_cell c with
    | None -> None
    | Some c' ->
        match get_adjacent_word c' st false [] with
      | None -> None
      | Some (str,_) ->
        let extensions = get_extensions (reverse_str str) r_dict in
        let words = (valid_extensions rack extensions) in
        Some (words,str) in
  let down_across =
      match down_cell c with
      | None -> None
      | Some c' ->
        if (get_cell_from_coordinate c' st |> cell_is_empty) then None
        else
          let letter = fst ((get_cell_from_coordinate c' st).letter) in
          let across_ext = get_extensions (Char.escaped letter) r_dict in
        let words = (valid_extensions rack across_ext) in
          Some (words, Char.escaped letter) in
  [left;right;up;down;left_across;right_across;up_across;down_across]

let all_moves anchors st =
  List.fold_left
    (fun acc x ->
       (fst x,(make_move (fst x) (snd x) st))::acc
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

let get_all_start_cells anchor word_lst st =
  let left =
    match List.nth word_lst 0 with
    | None -> []
    | Some pair ->
      (List.fold_left
        (fun acc x ->
           let updated_cell = (get_start_cell anchor (snd pair) Left) in
           (updated_cell, x)::acc
        ) [] (fst pair)) in
  let right =
    match List.nth word_lst 1 with
    | None -> []
    | Some pair ->
      List.fold_left
        (fun acc x ->
           let updated_cell = (get_start_cell anchor x Right) in
           let new_word = (reverse_str x ^ (snd pair)) in
           (updated_cell, new_word)::acc
        ) [] (fst pair) in
  let up =
    match List.nth word_lst 2 with
    | None -> []
    | Some pair ->
      (List.fold_left
        (fun acc x ->
           let updated_cell = (get_start_cell anchor (snd pair) Up) in
           (updated_cell, x)::acc
        ) [] (fst pair)) in
  let down =
    match List.nth word_lst 3 with
    | None -> []
    | Some pair ->
      List.fold_left
        (fun acc x ->
           let updated_cell = (get_start_cell anchor x Down) in
           let new_word = (reverse_str x ^ (snd pair)) in
           (updated_cell, new_word)::acc
        ) [] (fst pair) in
  let left_across =
    match List.nth word_lst 4 with
    | None -> []
    | Some pair ->
      (List.fold_left
        (fun acc x ->
           let updated_cell = (get_start_cell anchor (snd pair) Left) in
           (updated_cell, x)::acc
        ) [] (fst pair)) in
  let right_across =
    match List.nth word_lst 5 with
    | None -> []
    | Some pair ->
      List.fold_left
        (fun acc x ->
           let updated_cell = (get_start_cell anchor x Right) in
           let new_word = (reverse_str x ^ (snd pair)) in
           (updated_cell, new_word)::acc
        ) [] (fst pair) in
  let up_across =
    match List.nth word_lst 6 with
    | None -> []
    | Some pair ->
      (List.fold_left
        (fun acc x ->
           let updated_cell = (get_start_cell anchor (snd pair) Up) in
           (updated_cell, x)::acc
        ) [] (fst pair)) in
  let down_across =
    match List.nth word_lst 7 with
    | None -> []
    | Some pair ->
      List.fold_left
        (fun acc x ->
           let updated_cell = (get_start_cell anchor x Down) in
           let new_word = (reverse_str x ^ (snd pair)) in
           (updated_cell, new_word)::acc
        ) [] (fst pair) in
  [left @ right @ left_across @ right_across;
   up @ down @ up_across @ down_across]

let update_all_first_move_anchor_pairs anchor_pair_lst st =
  List.map (fun x -> get_all_first_move_start_cells
               (fst x) (snd x) st) anchor_pair_lst

let update_all_anchor_pairs anchor_pair_lst st =
    List.map (fun x -> get_all_start_cells (fst x) (snd x) st) anchor_pair_lst

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
        List.fold_left (fun acc (s, i) ->
            (fst acc  && check_word s st, snd acc + i)) (true, 0) word_score_lst
      in
      if fst valid_words then snd valid_words
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
      snd word_score

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

let get_all_move_points moves st =
  List.fold_left
    (fun acc x ->
       try
         ( x, get_points x st)::acc with
         _ -> acc
    ) [] moves

let get_all_first_move_points moves st =
  List.fold_left
    (fun acc x ->
       try
         ( x, get_first_move_points x st)::acc with
         _ -> acc
    ) [] moves

let score_cmp mv1 mv2 =
  if snd mv1 > snd mv2 then 1
  else if snd mv1 < snd mv2 then -1 else 0

let sort_moves moves =
  List.sort score_cmp moves

let evaluate_rack rack =
  List.fold_left
    (fun acc x ->
       if List.mem x vowels then (fst acc + 1, snd acc)
       else (fst acc, snd acc + 1)
    ) (0,0) rack

let do_swap rack st =
  if List.length (st.bag) <> 0 then Swap [List.hd rack]
  else Pass

let print_points lst =
  List.fold_right
    (fun x acc -> acc ^ (string_of_int (snd x )) ^ " " ) lst ""

let pick_best_move rack st moves =
  match moves with
  | [] -> do_swap rack st
  | _ ->
    (* let p = fst (List.sort score_cmp moves |> List.rev |> List.hd) in
    let pr = p.word in
    print_endline
      ( string_of_int(fst p.mv_coord) ^ "," ^ string_of_int(snd p.mv_coord) ^ " " ^
        string_of_bool(p.is_horizontal) ^ " " ^
          List.fold_right
         (fun x acc -> (Char.escaped x) ^ acc) pr ""); *)
    PlaceWord (fst (List.sort score_cmp moves |> List.rev |> List.hd))

let pick_worst_move rack st moves =
  match moves with
  | [] -> do_swap rack st
  | _ -> PlaceWord (fst (List.sort score_cmp moves |> List.hd))

let get_letters_rack rack =
  List.map(fun (letter,_) -> letter) rack

let first_move st =
  let letters_rack = st.current_player.rack |> get_letters_rack in
  let anchor = get_cell_from_coordinate (7,8) st in
  let anchor_moves = all_first_moves anchor letters_rack st in
  let updated_anchors = update_all_first_move_anchor_pairs anchor_moves st in
  let moves = generate_all_moves updated_anchors in
  (* print_endline (string_of_int (List.length moves)); *)
  get_all_first_move_points moves st |> pick_best_move letters_rack st

let best_move_helper st =
  let letters_rack = st.current_player.rack |> get_letters_rack in
  let all_cells = get_all_cells st in
  let empty_cells = get_empty_cells all_cells in
  let anchors = get_anchors empty_cells st in
  let anchor_pairs = generate_anchor_chars anchors letters_rack st in
  let anchor_moves = all_moves anchor_pairs st in
  let updated_anchors = update_all_anchor_pairs anchor_moves st in
  let moves = generate_all_moves updated_anchors in
  (* print_endline (string_of_int (List.length moves)); *)
  get_all_move_points moves st

let get_hint st =
  if List.for_all (fun p -> p.score = 0) st.players then first_move st
  else
    let letters_rack = st.current_player.rack |> get_letters_rack in
    best_move_helper st |> pick_worst_move letters_rack st

let best_move st =
  if List.for_all (fun p -> p.score = 0) st.players then first_move st
  else
    let letters_rack = st.current_player.rack |> get_letters_rack in
    best_move_helper st |> pick_best_move letters_rack st
