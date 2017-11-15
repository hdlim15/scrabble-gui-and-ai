type dictionary =
  | Empty
  | Node of char * (dictionary list) * (string option)

(* [explode s] is [s] split into a list of its characters, in order. *)
let explode s =
  let rec helper n lst =
    if n = 0 then lst
    else helper (n-1) (s.[n-1]::lst)
  in
  helper (String.length s) []

(* [remove_child n dict] is [dict] with [c] removed, where [c] is the character
 * of [n]. *)
let remove_child n dict =
  match n with
  | Empty -> failwith "cannot remove empty"
  | Node (c, _, _) ->
      List.filter
        (fun d -> match d with
         | Empty -> true
         | Node (c', _, _) -> c' <> c
        ) dict

let add_child n dict =
  match dict with
  | Empty -> failwith "cannot add child to Empty"
  | Node (c, lst, so) -> Node (c, n::(remove_child n lst), so)

(* [find c dict] is None, if [c] is not the char of a child of [dict]. If [c]
 * is the char of a child of dict, it returns Some child *)
let find c dict =
  match dict with
  | Empty -> None
  | Node (_, lst, _) ->
      List.find_opt (fun d ->
        match d with
        | Empty -> false
        | Node (c', _, _) -> c' = c
      ) lst

(* todo: adding a prefix as a word needs to be marked as a word *)
let insert dict w =
  let char_list = explode w in
  let rec helper lst acc =
    match lst with
    | [] -> acc
    | h::t ->
      begin
        match find h acc with
        | None ->
            let word_opt = if (t = []) then (Some w) else None in
            add_child (helper t (Node (h, [], word_opt))) acc
        | Some d' -> add_child (helper t d') acc
      end
  in
  helper char_list dict

let insert_word_list lst =
  (* [+] used to represent the head of the trie *)
  let root = Node ('+', [], None) in
  List.fold_left insert root lst


let is_word w dict =
  failwith "todo"

let get_subtree letter dict =
  failwith "todo"

(*
             *
            /
+ - h - e - l - l - o - *
     \
      a
       \
        t - e - *
         \
          * *)