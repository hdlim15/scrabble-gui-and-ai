type dictionary = unit


(* [insert w dict] is [dict] with [w] appended as a valid word in [dict] *)
val insert : string -> dictionary -> dictionary

(* [is_word w dict] is true iff [w] is as a valid word in [dict] *)
val is_word : string -> dictionary -> bool

(* [get_subtree c dict] is *)
val get_subtree : char -> dictionary -> dictionary