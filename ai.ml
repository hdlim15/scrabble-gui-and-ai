open Command
open Trie
open State

type direction = Left | Right | Up | Down

let f_dict = Trie.initialize_dict "forward_dict.txt"

let r_dict = initialize_dict "reverse_dict.txt"

let get_all_cells st =
  get_row (0,0) st @ get_row (1,0) st @ get_row (2,0) st @ get_row (3,0) st @
  get_row (4,0) st @ get_row (5,0) st @ get_row (6,0) st @ get_row (7,0) st @
  get_row (8,0) st @ get_row (9,0) st @ get_row (10,0) st @ get_row (11,0) st @
  get_row (12,0) st @ get_row (13,0) st @ get_row (14,0) st

let get_filled_cells lst =
  List.filter (fun c -> not(cell_is_empty c)) lst

let get_empty_cells lst =
  List.filter (fun c -> (cell_is_empty c)) lst

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

let adjacent_coordinates c =
  let up = up_cell c in
  let down = down_cell c in
  let left = left_cell c in
  let right = right_cell c in
  [up;down;left;right]

let has_adjacent_word_tile c st =
  List.fold_left
    (fun acc x ->
       match x with
       | None -> acc
       | Some t ->
       acc || (get_cell_from_coordinate t st |> cell_is_empty))
    false (adjacent_coordinates c)

let get_anchors empty_cells st =
  List.filter (fun c -> has_adjacent_word_tile c st ) empty_cells

let cross_check c chr cells st =
  let bool1 =
    match up_cell c with
    | None -> true
    | Some c' ->
      match get_adjacent_word c' st false [] with
      | None -> failwith "impossible"
      | Some (str,_) ->
        is_word f_dict (chr ^ str) in
  let bool2 =
    match down_cell c with
    | None -> true
    | Some c' ->
      match get_adjacent_word c' st false [] with
      | None -> failwith "impossible"
      | Some (str,_) ->
        is_word f_dict (chr ^ str) in
  bool1 || bool2

let anchor_chars anchor rack cells st =
  List.fold_left
    (fun acc x ->
       if cross_check anchor x cells st then x::acc else acc
    ) [] rack

let generate_anchor_chars anchors rack cells st =
  List.fold_left
    (fun acc x ->
       (x, anchor_chars x rack cells st )::acc
    ) [] anchors

let check_extension anchor_rack ext =
  List.fold_left
    (fun (check, rack') x ->
       check && List.mem x rack', remove x rack'
    ) (true,anchor_rack) (explode ext)

let valid_extensions anchor_rack extensions =
  List.fold_left
    (fun acc x ->
       let check = check_extension anchor_rack x in
       if fst check then x::acc else acc
    ) [] extensions

let reverse_str s =
  List.fold_right (fun x acc -> acc ^ Char.escaped x ) (explode s) ""

let concat_moves str exts =
  List.fold_left
    (fun acc x -> (str ^ x)::acc ) [] exts

let concat_moves_rev str exts =
  List.fold_left
    (fun acc x -> (x ^ str)::acc ) [] exts

let make_move c rack st =
  let left =
  match left_cell c with
  | None -> None
  | Some c' ->
      match get_adjacent_word c' st true [] with
    | None -> failwith "impossible"
    | Some (str,_) ->
      let extensions = get_extensions str f_dict in
      let words = (valid_extensions rack extensions) |> concat_moves str in
      Some (words,str) in
  let right =
  match right_cell c with
  | None -> None
  | Some c' ->
      match get_adjacent_word c' st true [] with
    | None -> failwith "impossible"
    | Some (str,_) ->
      let extensions = get_extensions (reverse_str str) r_dict in
      let words = (valid_extensions rack extensions) |> concat_moves_rev str in
      Some (words,str) in
  let up =
  match up_cell c with
  | None -> None
  | Some c' ->
      match get_adjacent_word c' st false [] with
    | None -> failwith "impossible"
    | Some (str,_) ->
      let extensions = get_extensions str f_dict in
      let words = (valid_extensions rack extensions) |> concat_moves str in
      Some (words,str)in
  let down =
    match down_cell c with
  | None -> None
  | Some c' ->
      match get_adjacent_word c' st false [] with
    | None -> failwith "impossible"
    | Some (str,_) ->
      let extensions = get_extensions (reverse_str str) r_dict in
      let words = (valid_extensions rack extensions) |> concat_moves_rev str in
      Some (words,str) in
  [left;right;up;down]

let all_moves anchors st =
  List.fold_left
    (fun acc x ->
       (x,(make_move (fst x) (snd x) st))::acc
    ) [] anchors

let get_start_cell anchor word dir =
  match dir with
  | Right | Down -> anchor.cell_coord
  | Left ->
    let subtract = String.length (word) in
    ((fst anchor.cell_coord) - subtract), snd anchor.cell_coord
  | Up ->
    let subtract = String.length (word) in
    fst anchor.cell_coord,((snd anchor.cell_coord) - subtract)

let get_all_start_cells anchor word_lst st =
  let left =
    List.fold_left
      (fun acc x ->
         let updated_cell = (get_start_cell anchor (snd x) Left) in
         (updated_cell, fst x)::acc
      ) [] (List.nth word_lst 0) in
  let right =
    List.fold_left
      (fun acc x ->
         let updated_cell = (get_start_cell anchor (snd x) Right) in
         (updated_cell, fst x)::acc
      ) [] (List.nth word_lst 1) in
  let up =
    List.fold_left
      (fun acc x ->
         let updated_cell = (get_start_cell anchor (snd x) Up) in
         (updated_cell, fst x)::acc
      ) [] (List.nth word_lst 2) in
  let down =
    List.fold_left
      (fun acc x ->
         let updated_cell = (get_start_cell anchor (snd x) Down) in
         (updated_cell, fst x)::acc
      ) [] (List.nth word_lst 3) in
  [left@right;up@down]

let update_all_anchor_pairs anchor_pair_lst st =
  List.map (fun x -> get_all_start_cells (fst x) (snd x) st) anchor_pair_lst

let get_points mv st =
  if not (check_bounds mv st) then raise (InvalidPlace "cannot place off board")
  else if not (check_endpoints mv st) then raise (InvalidPlace "not complete word")
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
            (fst acc, snd acc + i)) (true, 0) word_score_lst in
      if fst valid_words then snd valid_words
      else
        raise (InvalidPlace "invalid newly-formed word")

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

let calculate_move_points mv st =
  try get_points mv st with
    Failure _ -> failwith "invalid move"

let get_all_move_points moves st =
  List.fold_left
    (fun acc x ->
       try
         ( x, get_points x st)::acc with
        Failure _ -> acc
    ) [] moves

let score_cmp mv1 mv2 =
  if snd mv1 > snd mv2 then 1
  else if snd mv1 < snd mv2 then -1 else 0

let sort_moves moves =
  List.sort score_cmp moves

let pick_best_move moves =
  match moves with
  | [] -> failwith "swap"
  | _ -> List.sort score_cmp moves |> List.hd

let eval_move st mv =
  failwith "todo"

let best_move st =
  failwith "todo"
