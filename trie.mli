(* The type of the dictionary, which will be implemented later as a trie *)
type dictionary = unit

(* [insert w dict] is [dict] with [w] appended as a valid word in [dict] *)
val insert : string -> dictionary -> dictionary

(* [is_word w dict] is true iff [w] is as a valid word in [dict] *)
val is_word : string -> dictionary -> bool

(* [get_subtree c dict] is the subtree of [dict] with next letter [c] *)
val get_subtree : char -> dictionary -> dictionary