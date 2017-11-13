open State
open Command

val eval_move : State.state -> Command.move -> int

val best_move : State.state -> Command.move
