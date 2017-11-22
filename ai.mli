open Command
open State

type direction = Left | Right | Up | Down

(* [get_hint st] is the AI's worst choice of a move based on the
 * current state of the game.
 * Used for the 'hint' command.
 *)
val get_hint : State.state -> Command.command

(* [best_move st] is the AI's best choice of a move based on the
 * current state of the game *)
val best_move : State.state -> Command.command
