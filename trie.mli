(* The type of the dictionary, which will be implemented later as a trie *)
type dictionary = Node of char * (dictionary list) * bool

(* [get_subtree c dict] is [None] if no direct child of [dict] contains [c].
 * If there is a direct child of [dict], [child], that contains [c],
 * it returns [Some child] *)
val get_subtree : char -> dictionary -> dictionary option

(* [insert w dict] is [dict] with [w] appended as a valid word in [dict] *)
val insert : dictionary -> string -> dictionary

(* [is_word w dict] is true iff [w] is as a valid word in [dict] *)
val is_word : dictionary -> string -> bool


