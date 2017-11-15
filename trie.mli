(* The type of the dictionary, which will be implemented later as a trie *)
type dictionary = Node of char * (dictionary list) * bool

(* [insert w dict] is [dict] with [w] appended as a valid word in [dict] *)
val insert : dictionary -> string -> dictionary

(* [is_word w dict] is true iff [w] is as a valid word in [dict] *)
val is_word : string -> dictionary -> bool

(* [get_subtree c dict] is the subtree of [dict] with next letter [c] *)
val get_subtree : char -> dictionary -> dictionary
