open State
open Command
open Char
open Gui

(* [get_scores players] returns a string of player, score information *)
let rec get_scores players =
  let string_of_score p = p.name ^ ": " ^ (string_of_int p.score) in
  let score_string_list = (List.map string_of_score (List.rev players)) in
  String.concat "\n" score_string_list

(* [str_of_rack rack] is the string representation of a rack *)
let rec str_of_rack rack =
  rack |> List.map
    (fun (c,_) -> Char.escaped c ^ ":" ^ (string_of_int (State.get_points c)))
  |> (String.concat ", ")

(* [str_of_help ()] is the string representation of help.txt *)
let rec str_of_help () =
  let rec helper channel str =
    match (Pervasives.input_line channel) with
    | exception End_of_file -> Pervasives.close_in channel; str
    | s -> helper channel (str ^ "\n" ^ s)
  in
  helper (Pervasives.open_in "help.txt") ""

(* [string_of_board board] is a string representing the board state *)
let rec string_of_board b =
  let rec board_helper board =
    match board with
    | [] -> ""
    | row::t ->
      let rec helper row =
        match row with
        | [] -> ""
        | cell::[] -> " | " ^ Char.escaped (fst cell.letter) ^ " |"
        | cell::t ->
          if snd cell.cell_coord = 0 then
            let letter = Char.chr (fst cell.cell_coord + 65) in
            Char.escaped letter ^ " | " ^ Char.escaped (fst cell.letter) ^ helper t
          else
            " | " ^ Char.escaped (fst cell.letter) ^ helper t
      in helper row ^ "\n" ^ board_helper t
  in
  let board' = board_helper b in
  "  | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10| 11| 12| 13| 14|\n" ^ board'

(* [get_prev_player n p] returns the player whose turn was before the
 * current player with order number [n] given a list of the players [p]. *)
let rec get_prev_player n p =
  let n' =
    if n = 1 then List.length p
    else n - 1
  in
  List.hd (List.filter (fun p -> p.order_num = n') p)

(* [finalize_scores players] is the list of players, with their respective scores
 * updated based on the tiles left in their rack. Each player's score is reduced
 * by the sum of their unplayed letters. If a player played all their letters,
 * the sum of the other players' unplayed letters is added to thta player's
 * score. *)
let finalize_scores players =
  let empty_rack_player =
  if List.length (List.filter (fun p -> List.length p.rack = 0) players) <> 0 then
    Some (List.hd (List.filter (fun p -> List.length p.rack = 0) players))
  else None in
  let other_players = List.filter (fun p -> List.length p.rack <> 0) players in
  let score_helper s r =
    List.fold_left (fun acc (l,p) -> acc - p) s r in
  let other_players'_w_sumDeductedPts =
    List.fold_left (fun acc p ->
      let deducted_ps = p.score - score_helper p.score p.rack in
      ({p with score = p.score - deducted_ps}::(fst acc)), snd acc + deducted_ps) ([],0) other_players in
  match empty_rack_player with
  | Some p ->
    let empty_rack_player' = {p with score = p.score + snd other_players'_w_sumDeductedPts} in
    empty_rack_player' :: fst other_players'_w_sumDeductedPts
  | None -> fst other_players'_w_sumDeductedPts

(* [get_winner st] is the player with the highest score *)
let get_winner st =
  let players' = finalize_scores st.players in
  let winner_s = List.fold_left
      (fun acc p ->
         if p.score > (List.hd acc).score then [p]
         else
         if p.score = (List.hd acc).score then
             p::acc
           else acc) [List.hd players'] players' in
  if List.length winner_s = 1 then List.hd winner_s
  else List.fold_left (fun acc p ->
      if p.score > acc.score then p else acc) st.current_player st.players

(* [no_empty_rack st] is true if no player has an empty rack *)
let no_empty_rack st =
  List.for_all (fun p -> List.length p.rack <> 0) st.players

(* [get_command ()] is a command, generated from user input *)
let rec get_command st =
  match st.current_player.player_type with
  | Human ->
    (* print_string "> ";
    let command = try (parse (read_line ())) with
      | InvalidCommand ->
        (print_endline "Invalid action, type 'help' for a list of valid actions";
         get_command st)
       in command *)
    gui_cmd st
  | AI diff ->
    match diff with
    | Hard -> Ai.best_move st
    | Easy -> Ai.get_hint st

(* [clear ()] wipes the terminal window so players can't see other players' racks *)
let rec clear () =
  let rec helper n =
    if n = 0 then "\n" else "\n" ^ helper (n-1) in
  print_endline (helper 100);
  let _ = Sys.command "clear" in
  print_endline "S C R A B B L E";
  print_endline "For gameplay manual, type 'help'.\n";
  ()

(* [end_turn st end_type] ends a given turn and sets up the UI for the next player. *)
let end_turn st end_type =
  match end_type with
  | `Place ->
    print_endline (string_of_board st.board);
    print_endline "Press 'ENTER' to end your turn.";
    (* let _ = read_line () in *)
    clear ();
    print_endline (st.current_player.name ^ "'s turn.");
    (* print_endline (str_of_rack st.current_player.rack)  *)
  | `Swap ->
    print_endline (str_of_rack (get_prev_player st.current_player.order_num st.players).rack);
    print_endline "Press 'ENTER' to end your turn.";
    let _ = read_line () in
    clear ();
    print_endline (st.current_player.name ^ "'s turn.");
    (* print_endline (str_of_rack st.current_player.rack) *)
  | `Pass ->
    print_endline "Press 'ENTER' to end your turn.";
    let _ = read_line () in
    clear ();
    print_endline (st.current_player.name ^ "'s turn.")

(* [end_turn_gui st end_type] ends a given turn in the gui *)
let end_turn_gui st end_type =
  Graphics.set_color Graphics.black;
  match end_type with
  | `Place ->

    (* ensure redraw board *)

  Graphics.moveto 625 220;
  Graphics.draw_string "Press any key to end your turn";
  let _ = Graphics.wait_next_event [Graphics.Key_pressed] in ()
  | `Swap ->

    (* redraw rack so player can see it before ending turn *)

  Graphics.moveto 625 220;
  Graphics.draw_string "Press any key to end your turn";
  let _ = Graphics.wait_next_event [Graphics.Key_pressed] in ()
  | `Pass -> ()

(* [end_nonturn_command str] prints some output [str] to the gui and prompts the user
 * to press any key to continue their turn *)
let end_nonturn_command str =
  Graphics.set_color Graphics.black;
  Graphics.moveto 625 240;
  Graphics.draw_string str;
  Graphics.moveto 625 220;
  Graphics.draw_string "Press any key to continue your turn";
  let _ = Graphics.wait_next_event [Graphics.Key_pressed] in ()

(* [play_game st] plays the game represented by [st]. *)
let rec play_game st =
  let command = get_command st in
  let new_state =
    try
      match command with
      | PlaceWord mv ->
        let st' = do' command st in end_turn st' `Place; st'
      | Swap chars ->
        let st' = do' command st in end_turn_gui st' `Swap; st'
      | Score -> print_endline (get_scores st.players); st
      | Rack -> print_endline (str_of_rack st.current_player.rack); st
      | Hint ->
        let hint =
          match (Ai.get_hint st) with
          | PlaceWord mv ->
            "Hint: " ^ List.fold_right (fun x acc -> (Char.escaped x) ^ acc) mv.word ""
          | _ -> "Hint: you should swap or pass" in
        end_nonturn_command hint; st
      | AddWord str ->
        let st' = do' command st in end_nonturn_command ("Added word to dictionary"); st'
      | Help -> print_endline ((str_of_help ())^"\n"); st
      | Quit -> print_endline "Thanks for playing!\n"; exit 0;
      | Board -> print_endline (string_of_board st.board); st
      | Pass ->
        let st' = do' command st in end_turn_gui st' `Pass; st'
    with
    | InvalidPlace s -> end_nonturn_command ("Invalid Place: " ^ s); st
    | InvalidSwap ->
      if List.length st.bag = 0 then
        (end_nonturn_command "Cannot swap, bag is empty"; st)
      else (end_nonturn_command "Invalid swap"; st)
    | InvalidAdd -> end_nonturn_command ("Invalid Add"); st
  in
  update_gui new_state;
  if no_empty_rack new_state && new_state.sp_consec <= 12 then
    play_game new_state
  else
    (let winner = (get_winner new_state).name in
     print_endline ("Congratuations, " ^ winner ^ ", you win!\n");
     exit 0;)

(******************************************************************************)
(* GAME INITIALIZATION CODE *)

(* [init_player_names num_players] is a list of player names. *)
let rec init_player_names num_players =
  if num_players <> 0 then
    begin
      print_endline
        "\nEnter player names separated by spaces, in the order you want the turns to go.";
      print_string "> ";
      let player_names_str = read_line () in
      let player_name_lst = player_names_str |> Str.split (Str.regexp "[ \t]+") in
      if List.length player_name_lst <> num_players then
        (print_endline "Invalid number of names."; init_player_names num_players)
      else
      if List.length (List.sort_uniq Pervasives.compare player_name_lst) <> List.length player_name_lst then
        (print_endline "Player names must be unique."; init_player_names num_players)
      else
        player_name_lst
    end
  else
    []

(* [init_num_players ()] is the number of players (between 1 and 4). *)
let rec init_num_players () =
  print_endline "\nHow many players? Enter a number between 1 and 4.";
  print_string "> ";
  let num_players = read_line () in
  let num_players' = try int_of_string num_players with _ ->
    print_endline "Invalid entry."; init_num_players () in
  if num_players' > 4 || num_players' < 1 then
    (print_endline "Invalid number."; init_num_players ())
  else num_players'

(* [init_num_humans total_num_players] is the number of human players. *)
let rec init_num_humans total_num_players =
  print_endline "\nHow many human players?";
  print_string "> ";
  let num_humans = read_line () in
  let num_humans' = try int_of_string num_humans with _ ->
    print_endline "Invalid entry."; init_num_humans total_num_players in
  if num_humans' < 0 || num_humans' > total_num_players then
    (print_endline "Invalid number."; init_num_humans total_num_players)
  else num_humans'

(* [init_ai_diff num_ai] is a list of difficulties for a number [num_ai] of AIs *)
let rec init_ai_diff num_ai =
  print_endline ("\nAssign difficulty to " ^
                 (string_of_int num_ai) ^ " AI(s), separated by spaces.");
  print_endline "Difficulties are: easy, hard.";
  print_string "> ";
  let ai_diff = read_line () in
  let ai_diff_lst = ai_diff |> Str.split (Str.regexp "[ \t]+") in
  if List.length ai_diff_lst <> num_ai then
    (print_endline "Invalid number of difficulties."; init_ai_diff num_ai)
  else
    let rec helper lst =
      match lst with
      | [] -> []
      | h::t -> let h' = h |> String.trim |> String.lowercase_ascii in
        match h' with
        | "easy" -> Easy :: helper t
        | "hard" -> Hard :: helper t
        | _ -> (print_endline "Invalid difficulty entry."; failwith "") in
    try helper ai_diff_lst with Failure _ -> init_ai_diff num_ai

(* [init_game rdy], assuming rdy = "yes", gathers game state information from
 * user, generates an initial state [st], and then calls [play_game st]. *)
let init_game rdy =
  let rdy' = rdy |> String.trim |> String.lowercase_ascii in
  if rdy' <> "yes" then (print_endline "Quitting."; exit 0;)
  else
    let num_players = init_num_players () in
    let num_humans = init_num_humans num_players in
    let player_names = init_player_names num_humans in
    let ai_difficulty_lst =
      if num_humans <> num_players then
        init_ai_diff (num_players - num_humans)
      else [] in
    let s = init_state ({num_players = num_players;
                         num_humans = num_humans;
                         human_names = player_names;
                         ai_difficulty = ai_difficulty_lst}) in
    clear ();
    print_endline (s.current_player.name ^ "'s turn.");
    init_gui s;
    play_game s

(* [main ()] starts the REPL *)
let main () =
  try
    print_endline "\n\nWelcome to Scrabble!\n";
    print_endline "Ready to play? (yes/no)";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | ready -> init_game ready
  with
    Graphics.Graphic_failure _ -> print_endline "\nThanks for playing."; exit 0

let () = main ()
