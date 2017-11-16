type dictionary = Node of char * (dictionary list) * bool

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
  | Node (c, _, _) ->
      List.filter
        (fun d -> match d with
         | Node (c', _, _) -> c' <> c
        ) dict

let add_child n dict =
  match dict with
  | Node (c, lst, so) -> Node (c, n::(remove_child n lst), so)

(* [get_subtree c dict] is None, if [c] is not the char of a child of [dict].
 * If [c] is the char of a child of dict, it returns Some child *)
let get_subtree c dict =
  match dict with
  | Node (_, lst, _) ->
      List.find_opt (fun d ->
        match d with
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
        match get_subtree h acc with
        | None ->
            let new_node = Node (h, [], t=[]) in
            add_child (helper t new_node) acc
        | Some d' ->
            let new_node =
              if t = [] then let Node(c, dl, _) = d' in Node(c, dl, true)
              else d'
            in
            add_child (helper t new_node) acc
      end
  in
  helper char_list dict

let insert_word_list lst =
  (* [+] used to represent the head of the trie *)
  let root = Node ('+', [], false) in
  List.fold_left insert root lst


let is_word dict w =
  let char_list = explode w in
  let rec helper lst dict =
    match lst with
    | [] -> false
    | c :: [] ->
      begin
        match get_subtree c dict with
        | None -> false
        | Some (Node(_, _, b)) -> b
      end
    | c :: t ->
      begin
        match get_subtree c dict with
        | None -> false
        | Some d' -> helper t d'
      end
  in
  helper char_list dict

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