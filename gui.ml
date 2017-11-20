#require "graphics"
open Graphics

type relief = Top | Bot | Flat

type box_config =
  { x:int; y:int; w:int; h:int; bw:int; mutable r:relief;
    b1_col : Graphics.color;
    b2_col : Graphics.color;
    b_col : Graphics.color}

let draw_box_outline bcf col =
  Graphics.set_color col;
  draw_rect bcf.x bcf.y bcf.w bcf.h


let draw_box bcf =
  let x1 = bcf.x and y1 = bcf.y in
  let x2 = x1+bcf.w and y2 = y1+bcf.h in
  let ix1 = x1+bcf.bw and ix2 = x2-bcf.bw
  and iy1 = y1+bcf.bw and iy2 = y2-bcf.bw in
  let border1 g =
    Graphics.set_color g;
    Graphics.fill_poly
      [| (x1,y1);(ix1,iy1);(ix2,iy1);(ix2,iy2);(x2,y2);(x2,y1) |]
  in
  let border2 g =
  Graphics.set_color g;
  Graphics.fill_poly
  [| (x1,y1);(ix1,iy1);(ix1,iy2);(ix2,iy2);(x2,y2);(x1,y2) |]
  in
  Graphics.set_color bcf.b_col;
  (match bcf.r with
   | Top ->
    Graphics.fill_rect ix1 iy1 (ix2-ix1) (iy2-iy1);
    border1 bcf.b1_col;
    border2 bcf.b2_col;
  | Bot ->
    Graphics.fill_rect ix1 iy1 (ix2-ix1) (iy2-iy1);
    border1 bcf.b2_col;
    border2 bcf.b1_col;
  | Flat ->
    Graphics.fill_rect x1 y1 bcf.w bcf.h );
  draw_box_outline bcf Graphics.black

let erase_box bcf =
  Graphics.set_color bcf.b_col;
  Graphics.fill_rect (bcf.x+bcf.bw) (bcf.y+bcf.bw)
    (bcf.w-(2*bcf.bw)) (bcf.h-(2*bcf.bw))

type position = Left | Center | Right

let draw_string_in_box pos str bcf col =
  let (w, h) = Graphics.text_size str in
  let ty = bcf.y + (bcf.h-h)/2 in
  (match pos with
   | Center -> Graphics.moveto (bcf.x + (bcf.w-w)/2) ty
   | Right -> let tx = bcf.x + bcf.w - w - bcf.bw - 1 in
     Graphics.moveto tx ty
   | Left -> let tx = bcf.x + bcf.bw + 1 in Graphics.moveto tx ty );
  Graphics.set_color col;
  Graphics.draw_string str

let set_gray x = (Graphics.rgb x x x)

let gray1 = set_gray 100
and gray2 = set_gray 170
and gray3 = set_gray 240

let rec create_grid nb_col n sep b acc =
  if n < 0 then acc
  else
    let px = n mod nb_col and py = n / nb_col in
    let nx = b.x + sep + px * (b.w + sep)
    and ny = b.y + sep + py * (b.h + sep) in
    let b1 = {b with x = nx; y = ny} in
    (create_grid nb_col (n-1) sep b (b1 :: acc))

let vb =
  let b = {x=0; y=0; w=20;h=20; bw=2;
           b1_col=gray1; b2_col=gray3; b_col=gray2; r=Top} in
  Array.of_list (create_grid 15 224 0 b [])

let rec loop () = loop ()

let rec main () =
  try
    Graphics.open_graph " 600x600";
    Array.iter draw_box vb;
    draw_string_in_box Center "X" vb.(5) Graphics.black;
    draw_string_in_box Center "X" vb.(8) Graphics.black;
    draw_string_in_box Center "O" vb.(12) Graphics.yellow;
    draw_string_in_box Center "O" vb.(11) Graphics.yellow;
    draw_string_in_box Center "ABB" vb.(20) Graphics.yellow;
    loop ()
  with
  | Graphic_failure("fatal I/O error") ->
    print_endline "Thanks for playing!"
