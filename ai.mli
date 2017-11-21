open Command
open State

type direction = Left | Right | Up | Down

(* [best_move st] is an AI's choice of a move based on the
 * current state of the game *)
val best_move : State.state -> Command.command
