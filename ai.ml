open Command
open State

let get_all_cells st =
  get_row (0,0) st @ get_row (1,0) st @ get_row (2,0) st @ get_row (3,0) st @
  get_row (4,0) st @ get_row (5,0) st @ get_row (6,0) st @ get_row (7,0) st @
  get_row (8,0) st @ get_row (9,0) st @ get_row (10,0) st @ get_row (11,0) st @
  get_row (12,0) st @ get_row (13,0) st @ get_row (14,0) st

let get_filled_cells lst =
  List.filter (fun c -> not(cell_is_empty c)) lst

let get_empty_cells lst =
  List.filter (fun c -> (cell_is_empty c)) lst

let adjacent_coordinates c =
  let up =
  if fst (c.cell_coord ) = 0 then c.cell_coord
  else ((fst (c.cell_coord ) - 1), snd (c.cell_coord )) in
  let down =
  if fst (c.cell_coord ) = 14 then c.cell_coord
  else ((fst (c.cell_coord ) + 1), snd (c.cell_coord )) in
  let left =
  if snd (c.cell_coord ) = 0 then c.cell_coord
  else ((fst (c.cell_coord )), (snd (c.cell_coord )) - 1) in
  let right =
  if snd (c.cell_coord ) = 14 then c.cell_coord
  else ((fst (c.cell_coord )), (snd (c.cell_coord )) + 1) in
  [up;down;left;right]

let has_adjacent_word_tile c st =
  List.fold_left
    (fun acc x -> acc || (get_cell_from_coordinate x st |> cell_is_empty))
    false (adjacent_coordinates c)

let get_slots empty_cells st =
  List.filter (fun c -> has_adjacent_word_tile c st ) empty_cells


let eval_move st mv =
  failwith "todo"

let best_move st =
  failwith "todo"
