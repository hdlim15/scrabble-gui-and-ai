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
      match get_adjacent_word c' st false with
      | None -> failwith "impossible"
      | Some (str,_) ->
        is_word f_dict (chr ^ str) in
  let bool2 =
    match down_cell c with
    | None -> true
    | Some c' ->
      match get_adjacent_word c' st false with
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

let make_move c rack st =
  let left =
  match left_cell c with
  | None -> None, Left
  | Some c' ->
    match get_adjacent_word c' st true with
  | None -> failwith "impossible"
  | Some (str,_) ->
    let extensions = get_extensions str f_dict in
    Some (valid_extensions rack extensions), Left in
  let right =
  match right_cell c with
  | None -> None, Right
  | Some c' ->
    match get_adjacent_word c' st true with
  | None -> failwith "impossible"
  | Some (str,_) ->
    let extensions = get_extensions (reverse_str str) r_dict in
    Some (valid_extensions rack extensions), Right in
  let up =
  match up_cell c with
  | None -> None, Up
  | Some c' ->
    match get_adjacent_word c' st false with
  | None -> failwith "impossible"
  | Some (str,_) ->
    let extensions = get_extensions str f_dict in
    Some (valid_extensions rack extensions), Up in
  let down =
    match down_cell c with
  | None -> None, Down
  | Some c' ->
    match get_adjacent_word c' st false with
  | None -> failwith "impossible"
  | Some (str,_) ->
    let extensions = get_extensions (reverse_str str) r_dict in
    Some (valid_extensions rack extensions), Down in
  [left;right;up;down]

let all_moves anchors st =
  List.fold_left
    (fun acc x ->
       (make_move (fst x) (snd x) st)::acc
    ) [] anchors

let eval_move st mv =
  failwith "todo"

let best_move st =
  failwith "todo"
