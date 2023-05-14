val validate_king_move : Board.board -> Player.player -> int list -> bool
(** Given a board, a player and a list of coordinates, check whether a king can
    move to the destination specified in the coordinates. Takes into account
    castling as well.

    Precondition: Co-ordinates is a list of integers where the origin,
    (row,column) is specified by (lst[1], lst[0]). The destination is given by
    (lst[3],lst[2]). Precondtion: the Co-ordinates specified are valid and exist
    on the board given. *)
