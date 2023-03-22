(*Pawn movement Decision tree*)


type move =
| Normal of Board.board
| Final_row of Board.board
| Illegal

val move_pawn : Board.board-> Player.player -> int list -> move
