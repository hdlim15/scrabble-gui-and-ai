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

(* [add_child n dict] is [dict] with [n] appended as a child. If the character
 * of [n] is already contained in a direct child of [dict], [n] replaces that
 * child. *)
let add_child n dict =
  match dict with
  | Node (c, lst, so) -> Node (c, n::(remove_child n lst), so)

let get_subtree c dict =
  match dict with
  | Node (_, lst, _) ->
      List.find_opt (fun d ->
        match d with
        | Node (c', _, _) -> c' = c
      ) lst

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

let is_word dict w =
  let char_list = explode w in
  let rec helper lst dict =
    match lst with
    | [] -> false
    | c :: t ->
      begin
        match get_subtree c dict with
        | None -> false
        | Some d' ->
            if (t = []) then let (Node(_, _, b)) = d' in b
            else (helper t d')
      end
  in
  helper char_list dict

(* [insert_word_list lst] is the dictionary that results from adding all the
 * words in [lst] to the empty dictionary. *)
let insert_word_list lst =
  (* [+] used to represent the head of the trie *)
  let root = Node ('+', [], false) in
  List.fold_left insert root lst

(* [initialize_dict file] is the dictionary that results from adding all the
 * words in [file] to the empty dictionary.
 * requires: there is one word per line in [file]
 * aside: if you uncomment the commented code, it will also return a list of
 * all the words, which is useful for utop debugging *)
let initialize_dict file =
  let rec helper channel dict (* lst *) =
    match (Pervasives.input_line channel) with
    (* If End_of_file, close file and return idx' *)
    | exception End_of_file -> Pervasives.close_in channel ; dict(* , lst *)
    | s -> helper channel (insert dict s) (* (s::lst) *)
  in
  helper (Pervasives.open_in file) (Node ('+', [], false)) (* [] *)
