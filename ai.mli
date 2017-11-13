open State
open Command

type ai

val eval_move : State.state -> Command.move -> int

val best_move : State.state -> Command.move
