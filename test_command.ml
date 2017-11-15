open OUnit2
open Command

let tests = [
  (* Quit tests. *)
  "quit_lower" >:: (fun _ -> assert_equal Quit (parse "quit"));
  "quit_upper" >:: (fun _ -> assert_equal Quit (parse "QUIT"));
  "quit_space" >:: (fun _ -> assert_equal Quit (parse " QUIT "));


  (* Help tests. *)
  "help_lower" >:: (fun _ -> assert_equal Help (parse "help"));
  "help_upper" >:: (fun _ -> assert_equal Help (parse "HELP"));
  "help_space" >:: (fun _ -> assert_equal Help (parse " help "));


  (* Hint tests. *)
  "hint_lower" >:: (fun _ -> assert_equal Hint (parse "hint"));
  "hint_upper" >:: (fun _ -> assert_equal Hint (parse "HINT"));
  "hint_space" >:: (fun _ -> assert_equal Hint (parse " hint "));


  (* Score tests. *)
  "score_lower" >:: (fun _ -> assert_equal Score (parse "score"));
  "score_upper" >:: (fun _ -> assert_equal Score (parse "SCORE"));
  "score_space" >:: (fun _ -> assert_equal Score (parse " SCORE  "));


  (* Rack tests. *)
  "rack_lower" >:: (fun _ -> assert_equal Rack (parse "rack"));
  "rack_upper" >:: (fun _ -> assert_equal Rack (parse "RACK"));
  "rack_space" >:: (fun _ -> assert_equal Rack (parse "  RACK "));


  (* AddWord tests. *)
  "add_lower" >:: (fun _ -> assert_equal (AddWord "blah") (parse "add blah"));
  "add_upper" >:: (fun _ -> assert_equal (AddWord "blah") (parse "ADD BLAH"));
  "add_space" >:: (fun _ -> assert_equal (AddWord "blah") (parse " ADD   BLAH "));


  (* Swap tests. *)
  "swap_lower" >:: (fun _ -> assert_equal (Swap ['a';'b';'c']) (parse "swap abc"));
  "swap_upper" >:: (fun _ -> assert_equal (Swap ['d']) (parse "SWAP D"));
  "swap_space" >:: (fun _ -> assert_equal (Swap ['d']) (parse " SWAP   D  "));


  (* PlaceWord tests. *)
  "place_lower_horizontal" >:: (fun _ ->
      assert_equal (PlaceWord {word_segment=['a';'b';'c']; coordinate=(0,0);
                               is_horizontal=true}) (parse "place abc a0 horizontal"));
  "place_upper_horizontal" >:: (fun _ ->
      assert_equal (PlaceWord {word_segment=['d']; coordinate=(10,10);
                               is_horizontal=true}) (parse "PLACE D K10 HORIZONTAL"));
  "place_vertical" >:: (fun _ ->
      assert_equal (PlaceWord {word_segment=['a';'b']; coordinate=(1,13);
                               is_horizontal=false}) (parse "place ab b13 vertical"));
  "place_spaces_vertical" >:: (fun _ ->
      assert_equal (PlaceWord {word_segment=['a';'b']; coordinate=(1,13);
                               is_horizontal=false}) (parse " place   ab  b13  vertical "));

  (* Exception tests. *)
  "invalid_command" >:: (fun _ -> let e = fun () -> parse "do something" in
                          assert_raises InvalidCommand e);
  "no_args_add" >:: (fun _ -> let e = fun () -> parse "add" in
                      assert_raises InvalidCommand e);
  "no_args_swap" >:: (fun _ -> let e = fun () -> parse "swap" in
                       assert_raises InvalidCommand e);
  "no_args_place" >:: (fun _ -> let e = fun () -> parse "place" in
                        assert_raises InvalidCommand e);
  "wrong_args_1_place" >:: (fun _ -> let e = fun () -> parse "place a" in
                             assert_raises InvalidCommand e);
  "wrong_args_2_place" >:: (fun _ -> let e = fun () -> parse "place a i1" in
                             assert_raises InvalidCommand e);
]
