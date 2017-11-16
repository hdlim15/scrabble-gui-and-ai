open OUnit2
open Trie

(* [file_word_list file] is the list of words found in [file]. *)
let file_word_list file =
  let rec helper channel lst =
    match (Pervasives.input_line channel) with
    (* If End_of_file, close file and return idx' *)
    | exception End_of_file -> Pervasives.close_in channel ; lst
    | s -> helper channel (s::lst)
  in
  helper (Pervasives.open_in file) []

(* [test_dict dict file] is true iff [is_word dict w] is true for all words [w]
 * in [file] that were inserted into a dictionary [dict]. *)
let test_dict file =
  let dict = initialize_dict file in
  let word_list = file_word_list file in
  (List.filter (is_word dict) word_list) = word_list

let tests = [
  "forward_dict" >:: (fun _ -> assert_equal true (test_dict "forward_dict.txt"));
  "reverse_dict" >:: (fun _ -> assert_equal true (test_dict "reverse_dict.txt"));
]