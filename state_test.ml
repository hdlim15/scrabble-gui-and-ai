open OUnit2

(* EXAMPLE *)
(* let inv_t = [
  (* Test current inventory = starting inventory at beginning of game for various
   * Adventure Files *)
  "it_oneroom" >:: (fun _ -> assert_equal [] (oneroom |> init_state |> inv));
  "it_threerooms" >:: (fun _ ->
      assert_equal ["white hat"] (threerooms |> init_state |> inv));
  "it_gates" >:: (fun _ ->
      assert_equal ["companion cube"] (gates |> init_state |> inv));
] *)

let suite =
  failwith "todo, example syntax below"
  (* "Adventure test suite" >::: List.flatten [
    init_state_t;
    win_score_t;
    score_t;
    turns_t;
    current_room_id_t;
    inv_t;
    visited_t;
    locations_t;
    do'_t;
  ] *)

let _ = run_test_tt_main suite
