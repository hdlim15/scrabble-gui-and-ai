open OUnit2
open Ai
open State

let lower_left = {cell_coord = (1, 0); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1}
let lower_right = {cell_coord = (1, 1); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 2}
let upper_left = {cell_coord = (0, 0); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 3}
let upper_right = {cell_coord = (0, 1); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1}

let board = [[upper_left;upper_right];[lower_left;lower_right]]

let hard_ai = {name = "hard_ai";
               score = 0;
               rack = [('s', 1);('p', 3);('s', 1);('o', 1);('p', 3);('p', 3);('u', 1)];
               player_type = AI Hard;
               order_num = 1}

let easy_ai = {name = "easy_ai";
               score = 0;
               rack = [('s', 1);('p', 3);('s', 1);('o', 1);('p', 3);('p', 3);('u', 1)];
               player_type = AI Easy;
               order_num = 1}

let hard_state = {board = init_board 15;
                        bag = [('z', 10)];
                        players = [hard_ai];
                        added_words = [];
                        current_player = hard_ai;
                        is_first_move = true;
                        sp_consec = 0}

let easy_state = {board = init_board 15;
                        bag = [('z', 10)];
                        players = [easy_ai];
                        added_words = [];
                        current_player = easy_ai;
                        is_first_move = true;
                        sp_consec = 0}

let tests = [
  "reverse_empty" >:: (fun _ -> assert_equal "" (reverse_str ""));
  "reverse_str" >:: (fun _ -> assert_equal "abcd" (reverse_str "dcba"));

  "up_cell_some1" >:: (fun _ -> assert_equal (Some (0,0)) (up_cell lower_left));
  "up_cell_some2" >:: (fun _ -> assert_equal (Some (0,1)) (up_cell lower_right));
  "up_cell_none1" >:: (fun _ -> assert_equal None (up_cell upper_left));
  "up_cell_none2" >:: (fun _ -> assert_equal None (up_cell upper_right));

  "left_cell_some1" >:: (fun _ -> assert_equal (Some (0,0)) (left_cell upper_right));
  "left_cell_some2" >:: (fun _ -> assert_equal (Some (1,0)) (left_cell lower_right));
  "left_cell_none1" >:: (fun _ -> assert_equal None (left_cell upper_left));
  "left_cell_none2" >:: (fun _ -> assert_equal None (left_cell lower_left));

  "down_cell_some1" >:: (fun _ -> assert_equal (Some (1,0)) (down_cell upper_left));
  "down_cell_some2" >:: (fun _ -> assert_equal (Some (1,1)) (down_cell upper_right));

  "right_cell_some1" >:: (fun _ -> assert_equal (Some (0,1)) (right_cell upper_left));
  "right_cell_some2" >:: (fun _ -> assert_equal (Some (1,1)) (right_cell lower_left));

]
