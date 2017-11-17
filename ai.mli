open Command
open State

(* [eval_move st mv] is an AI's quantitative evaluation of a move based on the
 * current state of the game *)
val eval_move : State.state -> Command.move -> int

(* [best_move st] is an AI's choice of a move based on the
 * current state of the game *)
val best_move : State.state -> Command.move
