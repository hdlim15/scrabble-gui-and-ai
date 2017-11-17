open Command
open State

let get_all_cells st =
  get_row (0,0) st @ get_row (1,0) st @ get_row (2,0) st @ get_row (3,0) st @
  get_row (4,0) st @ get_row (5,0) st @ get_row (6,0) st @ get_row (7,0) st @
  get_row (8,0) st @ get_row (9,0) st @ get_row (10,0) st @ get_row (11,0) st @
  get_row (12,0) st @ get_row (13,0) st @ get_row (14,0) st

let get_filled_cells lst =
  List.filter (fun c -> not(cell_is_empty c)) lst

let eval_move st mv =
  failwith "todo"

let best_move st =
  failwith "todo"
