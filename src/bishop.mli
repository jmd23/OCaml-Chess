(*Bishop movement Decision tree*)

val validate_bishop_move : Board.board -> Player.player -> int list -> bool
(** Given a board, a player and a list of coordinates, check whether a bishop
    can move to the destination specified in the coordinates.

    Precondition: Co-ordinates is a list of integers where the origin,
    (row,column) is specified by (lst[1], lst[0]). The destination is given by
    (lst[3],lst[2]). Precondtion: the Co-ordinates specified are valid and exist
    on the board given. *)
