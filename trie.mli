type letter = char

type dictionary = unit

val insert : letter list -> dictionary -> dictionary

val is_word : letter list -> dictionary -> bool

val get_subtree : letter -> dictionary -> dictionary