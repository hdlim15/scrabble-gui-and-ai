open State
open Command
open Char

(* [get_scores players] returns a string of player, score information *)
let rec get_scores players =
  let string_of_score p = p.name ^ ": " ^ (string_of_int p.score) in
  let score_string_list = (List.map string_of_score (List.rev players)) in
  String.concat "\n" score_string_list

(* [str_of_rack rack] is the string representation of a rack *)
let rec str_of_rack rack =
  rack |> List.map (fun (c,_) -> Char.escaped c) |> (String.concat ", ")

(* [str_of_help ()] is the string representation of help.txt *)
let rec str_of_help () =
  let rec helper channel str =
    match (Pervasives.input_line channel) with
    | exception End_of_file -> Pervasives.close_in channel; str
    | s -> helper channel (str ^ "\n" ^ s)
  in
  helper (Pervasives.open_in "help.txt") ""

let rec get_command () =
  print_string "> ";
  let command = try (parse (read_line ())) with
    | InvalidCommand -> (print_endline "Invalid action, try again"; get_command ())
  in command

(* [play_game st] plays the game represented by [st]. *)
let rec play_game st =
  let command = get_command () in
  let new_state =
    try
      match command with
      | PlaceWord mv -> failwith "todo main.ml PlaceWord"
      | Swap chars -> print_endline "Swapping!"; do' command st
      | Score -> print_endline (get_scores st.players); st
      | Rack -> print_endline (str_of_rack st.current_player.rack); st
      | Hint -> failwith "todo main.ml Hint (requires AI code)"
      | AddWord str -> print_endline ("Added word to dictionary!"); do' command st
      | Help -> print_endline ((str_of_help ())^"\n"); st
      | Quit -> print_endline "Thanks for playing!\n"; exit 0;
    with
    | _ -> failwith "catch all possibly-raised exceptions"
  in
  (* MUST CHECK WIN/ENDGAME CONDITION HERE *)
  play_game new_state


(* GAME INITIALIZATION CODE BELOW *)

(* [init_player_names num_players] is a list of player names. *)
let rec init_player_names num_players =
  print_endline "\nEnter player names separated by spaces, in the order you want the turns to go.";
  print_string "> ";
  let player_names_str = read_line () in
  let player_name_lst = player_names_str |> Str.split (Str.regexp "[ \t]+") in
  if List.length player_name_lst <> num_players then
    (print_endline "Invalid number of names."; init_player_names num_players)
  else player_name_lst

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
  print_endline ("\nAssign difficulty to " ^ (string_of_int num_ai) ^ " AI(s), separated by spaces.");
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
        | _ -> (print_endline "Invalid difficulty entry."; init_ai_diff num_ai) in
    helper ai_diff_lst

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
    print_endline "For gameplay manual, type 'help'.";
    print_endline "Let's begin!";
    play_game s

(* [main ()] starts the REPL *)
let main () =
  print_endline "\n\nWelcome to Scrabble!\n";
  print_endline "Ready to play? (yes/no)";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | ready -> init_game ready

let () = main ()
