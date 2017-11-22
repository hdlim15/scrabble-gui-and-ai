open State

(* [relief] represents a particular area of a scrabble board tile. Different
 * areas are shaded differently to provide a 3D effect. *)
type relief =
  | Top
  | Bot
  | Flat

(* [box_config] represents a configuration for a box drawn on the gui. It holds
 * all of the necessary information to draw a cell of the scrabble grid on the
 * gui. *)
type box_config =
  {x : int;
   y : int;
   w : int;
   h : int;
   bw : int;
   mutable r : relief;
   b1_col : Graphics.color;
   b2_col : Graphics.color;
   b_col : Graphics.color}

(* [get_chars s] splits [s] into a list of its characters. *)
let get_chars s =
  let rec split_helper s' acc =
    match s' with
    | "" -> acc
    | s'' -> split_helper (String.sub s'' 1 ((String.length s'') - 1))
               (String.get s'' 0 :: acc)
  in List.rev (split_helper s [])

(* [draw_string s x y is_h] draws string [s] in direction [is_h], with the
 * starting bottom-left coordinate at (x, y). *)
let draw_string s x y is_h =
  let rec draw_string_helper lst x y =
    match lst with
    | [] -> ()
    | h::t ->
      begin
        if is_h then
          let text_width = fst (Graphics.text_size (Char.escaped h)) in
          Graphics.moveto x y;
          Graphics.draw_char h;
          draw_string_helper t (x + text_width) y
        else
          let text_height = snd (Graphics.text_size (Char.escaped h)) in
          Graphics.moveto x y;
          Graphics.draw_char h;
          draw_string_helper t x (y - text_height)
      end
  in
  Graphics.set_color Graphics.black;
  draw_string_helper (get_chars s) x y

(* [draw_box_outline bcf col] draws the outline of a box with color [col]
 * and box_config [bcf]. *)
let draw_box_outline bcf col =
  Graphics.set_color col;
  Graphics.draw_rect bcf.x bcf.y bcf.w bcf.h

(* [draw_box bcf] draws and fills in a box with box_config [bcf]. *)
let draw_box bcf =
  let x1 = bcf.x in
  let y1 = bcf.y in
  let x2 = x1 + bcf.w in
  let y2 = y1 + bcf.h in
  let ix1 = x1 + bcf.bw in
  let ix2 = x2 - bcf.bw in
  let iy1 = y1 + bcf.bw in
  let iy2 = y2 - bcf.bw in
  let border1 g =
    Graphics.set_color g;
    Graphics.fill_poly
      [|(x1, y1); (ix1, iy1); (ix2, iy1); (ix2, iy2); (x2, y2); (x2, y1)|]
  in
  let border2 g =
    Graphics.set_color g;
    Graphics.fill_poly
      [|(x1, y1); (ix1, iy1); (ix1, iy2); (ix2, iy2); (x2, y2); (x1, y2)|]
  in
  Graphics.set_color bcf.b_col;
  (match bcf.r with
   | Top ->
    Graphics.fill_rect ix1 iy1 (ix2 - ix1) (iy2 - iy1);
    border1 bcf.b1_col;
    border2 bcf.b2_col;
   | Bot ->
     Graphics.fill_rect ix1 iy1 (ix2 - ix1) (iy2 - iy1);
     border1 bcf.b2_col;
     border2 bcf.b1_col;
   | Flat ->
     Graphics.fill_rect x1 y1 bcf.w bcf.h);
  draw_box_outline bcf Graphics.black

let erase_box bcf =
  Graphics.set_color bcf.b_col;
  Graphics.fill_rect (bcf.x + bcf.bw) (bcf.y + bcf.bw)
    (bcf.w - (2 * bcf.bw)) (bcf.h - (2 * bcf.bw))

(* [position] represents the starting position for displaying a drawing, such as
 * a string, in a cell box. This determines whether the drawing is placed in
 * the left, center, or right side. *)
type position =
  | Left
  | Center
  | Right

(* [draw_string_in_box pos str bcf col] draws string [str] with color [col]
 * placed at position [pos] in a box with box_config [bcf]. *)
let draw_string_in_box pos str bcf col =
  let (w, h) = Graphics.text_size str in
  let ty = bcf.y + (bcf.h - h) / 2 in
  (match pos with
   | Center -> Graphics.moveto (bcf.x + (bcf.w - w)/2) ty
   | Right -> let tx = bcf.x + bcf.w - w - bcf.bw - 1 in
     Graphics.moveto tx ty
   | Left -> let tx = bcf.x + bcf.bw + 1 in Graphics.moveto tx ty);
  Graphics.set_color col;
  Graphics.draw_string str

(* [set_rgb r g b] returns a Graphics color with red component [r], green
 * component [g], and blue component [b]. *)
let set_rgb r g b = (Graphics.rgb r g b)

let beige1 = set_rgb 185 185 165
let beige2 = set_rgb 205 205 185
let beige3 = set_rgb 245 245 220

let dark_beige1 = set_rgb 155 155 140
let dark_beige2 = set_rgb 170 170 135
let dark_beige3 = set_rgb 205 205 175

let red1 = set_rgb 255 20 20
let red2 = set_rgb 250 65 65
let red3 = set_rgb 250 100 100

let green1 = set_rgb 20 140 20
let green2 = set_rgb 80 190 80
let green3 = set_rgb 120 230 120

let blue1 = set_rgb 35 85 185
let blue2 = set_rgb 65 115 215
let blue3 = set_rgb 130 170 255

let orange1 = set_rgb 240 140 15
let orange2 = set_rgb 255 170 60
let orange3 = set_rgb 255 195 110

let gray1 = set_rgb 120 120 120
let gray2 = set_rgb 160 160 160
let gray3 = set_rgb 210 210 210

(* [tws_indeces] are the array indices that correspond to cells with triple
 * word multipliers. *)
let tws_indeces =
  [0; 7; 14; 105; 119; 210; 217; 224]

(* [dls_indeces] are the array indices that correspond to cells with double
 * letter multipliers. *)
let dls_indeces =
  [3; 11; 36; 38; 45; 52; 59; 92; 96; 98; 102; 108; 116; 122; 126; 128; 132;
   165; 172; 179; 186; 188; 213; 221]

(* [dws_indeces] are the array indices that correspond to cells with double
 * word multipliers. *)
let dws_indeces =
  [16; 28; 32; 42; 48; 56; 64; 70; 154; 160; 168; 176; 182; 192; 196; 208]

(* [tls_indeces] are the array indices that correspond to cells with triple
 * letter multipliers. *)
let tls_indeces =
  [20; 24; 76; 80; 84; 88; 136; 140; 144; 148; 200; 204]

(* [create_grid nb_col n sep b acc] creates a grid of boxes of the same size.
 * [nb_col] corresponds to the number of columns in the grid, [n] corresponds to
 * the total number of boxes that are created in the grid, [sep] corresponds to
 * the seperation between boxes in the grid, and [b] is a box_config for the box
 * being created. A list of box_configs corresponding to each box in the grid is
 * returned. *)
let rec create_grid nb_col n sep b acc =
  if n < 0 then acc
  else
    let px = n mod nb_col in
    let py = n / nb_col in
    let nx = b.x + sep + px * (b.w + sep) in
    let ny = b.y + sep + py * (b.h + sep) in
    let b' =
      if List.mem n tws_indeces then
        {b with x = nx; y = ny; b1_col = orange1; b2_col = orange3; b_col = orange2}
      else if List.mem n dls_indeces then
        {b with x = nx; y = ny; b1_col = blue1; b2_col = blue3; b_col = blue2}
      else if List.mem n dws_indeces then
        {b with x = nx; y = ny; b1_col = red1; b2_col = red3; b_col = red2}
      else if List.mem n tls_indeces then
        {b with x = nx; y = ny; b1_col = green1; b2_col = green3; b_col = green2}
      else
        {b with x = nx; y = ny}
    in
    (create_grid nb_col (n - 1) sep b (b' :: acc))

(* [vb] corresponds to the array of box_configs that represent the board grid. *)
let vb =
  let b = {x = 0; y = 0; w = 40; h = 40; bw = 2;
           b1_col = beige1; b2_col = beige3; b_col = beige2; r = Top} in
  Array.of_list (create_grid 15 224 0 b [])

(* [board_to_graph_row n] returns the graph row coordinate given the board
 * row coordinate [n].
 * - example: (0, 0) in state represents the top, left cell, but when creating
 * the GUI, it represents the bottom, left cell (14, 0). *)
let board_to_graph_row n =
  14 - n

(* [coord_to_array_index c] returns the array index in the scrabble grid
 * corresponding to coordinate [c]. *)
let coord_to_array_index c =
  match c with
  | (x, y) -> 15 * (board_to_graph_row x) + y

(* [create_rack n b acc] creates boxes for each letter in the current player's
 * rack. [n] corresponds to the number of letters and [b] corresponds to the
 * box_config for the box being created. *)
let rec create_rack n b acc =
  if n < 0 then acc
  else
    let x' = b.x + b.w in
    let b' = {b with x = x'} in
    create_rack (n - 1) b' (b' :: acc)

(* [rack n] corresponds to the array of box_configs that represents the current
 * player's rack. There are [n] letters in the rack. *)
let rack n =
  let b = {x = 620; y = 350; w = 40; h = 40; bw = 2;
           b1_col = beige1; b2_col = beige3; b_col = beige2; r = Top} in
  Array.of_list (create_rack (n - 1) b [])

(* [update_rack cp] takes the current player [cp] and draws their rack on the
 * board so the player can see their tiles. Generates boxes for each letter. *)
let update_rack cp =
  let len_rack = List.length cp.rack in
  let r_array = rack len_rack in
  let rec update_rack_helper r n =
    match r with
    | [] -> ()
    | (l, p)::t ->
      begin
        if l <> ' ' then
          let tile_str =
            String.capitalize_ascii (Char.escaped l) ^ " : " ^ (string_of_int p)
          in
          draw_string_in_box Center tile_str r_array.(n) Graphics.black;
          update_rack_helper t (n + 1)
        else
          update_rack_helper t (n + 1)
      end
  in
  draw_string (cp.name ^ "'s rack:") 660 400 true;
  Array.iter draw_box r_array;
  update_rack_helper cp.rack 0

let init_pass () =
  let b = {x = 630; y = 30; w = 60; h = 60; bw = 2;
           b1_col = gray1; b2_col = gray3; b_col = gray2; r = Top} in
  draw_box b;
  draw_string_in_box Center "Pass" b Graphics.black

(* [update_vb b] takes a flattened board [b] and replaces any colored cells with
 * letters in them with standard beige colors in [vb]. This represents a
 * multiplier being used up. *)
let rec update_vb b =
  match b with
  | [] -> ()
  | cell::t ->
    if fst cell.letter <> ' ' then
      begin
        let array_idx = coord_to_array_index cell.cell_coord in
        let array_cell = Array.get vb array_idx in
        let array_cell' = {array_cell with b1_col = dark_beige1;
                                           b2_col = dark_beige3;
                                           b_col = dark_beige2}
        in
        Array.set vb array_idx array_cell';
        update_vb t
      end
    else
      update_vb t

(* [update_board b] updates the GUI with changes in the flattened board [b]
 * whenever a move is made. *)
let update_board b =
  let rec update_board_helper b' =
    match b' with
    | [] -> ()
    | cell::t ->
      if fst cell.letter <> ' ' then
        begin
          draw_string_in_box Center (String.capitalize_ascii (Char.escaped (fst cell.letter)))
            vb.(coord_to_array_index (cell.cell_coord)) Graphics.black;
          update_board_helper t
        end
      else
        update_board_helper t
  in
  Array.iter draw_box vb;
  update_board_helper b

(* [draw_logo ()] draws the Scrabble logo at the top of the GUI at the start of
 * each new game. *)
let draw_logo () =
  let bar_height = snd (Graphics.text_size "|") in
  draw_string "                               _       _       _         " 625 575 true;
  draw_string "  ___    ___    _ __    __ _  | |__   | |__   | |   ___  " 625 (575 - bar_height) true;
  draw_string " / __|  / __|  | '__| / _` | |  _ \\ |  _ \\ | |  / _ \\ " 625 (575 - 2 * bar_height) true;
  draw_string " \\__\\| (__   | |    | (_| | | |_) | | |_) | | | |  __/ " 625 (575 - 3 * bar_height) true;
  draw_string " |___/  \\___| |_|    \\__,_| |_.__/  |_.__/  |_|  \\___|" 625 (575 - 4 * bar_height) true;
  draw_string " ________________________________________________________" 625 (575 - 5 * bar_height) true


(* [compare_players p1 p2] compares the order numbers of players [p1] and [p2]. *)
let compare_players p1 p2 =
  if p1.order_num < p2.order_num then 1 else -1

(* [update_scores ps] takes a list of players [ps] and draws the scores of each
 * player on the GUI after each turn. *)
let update_scores ps =
  let sorted_ps = List.sort compare_players ps in
  let h = snd (Graphics.text_size "|") in
  let w = fst (Graphics.text_size "w") in
  draw_string "Scores:" 625 (575 - 7 * h) true;
  let rec helper ps' =
    match ps' with
    | [] -> ()
    | p::t -> draw_string (p.name ^ ": " ^ (string_of_int p.score))
                632 (575 - (8 + (List.length t)) * h) true;
              helper t
  in helper sorted_ps;
  let max_name_len = List.fold_left
      (fun acc p -> if String.length p.name > acc then
                      String.length p.name
                    else acc) 0 ps in
  let box_width = w * max_name_len + 50 in
  let box_height = h * (List.length ps + 1) + 5 in
  Graphics.draw_rect 620 (575 - box_height - 6 * h) box_width box_height;
  Graphics.set_color Graphics.blue

(* [update_gui st] takes the current state [st] and updates the GUI after each
 * move is made. *)
let update_gui st =
  Graphics.clear_graph ();
  draw_logo ();
  update_vb (List.flatten st.board);
  update_board (List.flatten st.board);
  update_scores st.players;
  update_rack st.current_player;
  init_pass ()

(* [init_gui st] initializes the GUI when the game starts with initial state
 * [st]. The graphics window is opened and the empty board, logo, scoreboard,
 * first player's rack, and buttons are drawn. *)
let init_gui st =
  try
    Graphics.open_graph " 1000x600";
    Graphics.set_window_title "Scrabble";
    Array.iter draw_box vb;
    Array.iter draw_box (rack 7);
    update_scores (st.players);
    update_rack (st.current_player);
    draw_string_in_box Center "START" vb.(112) Graphics.black;
    draw_logo ();
    init_pass ()
  with
  | Graphics.Graphic_failure("fatal I/O error") ->
    print_endline "Thanks for playing!"
