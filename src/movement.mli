exception Invalid_move
exception Invalid_piece

val move_piece : Board.board -> Player.player -> int list -> Board.board
(** [move_piece brd plr lst] moves the player's,[plr], piece at lst[1],lst[0] on
    board, [brd]. Returns a new board with piece at lst[4],lst[3]

    - Raises [Invalid_move] if origin position specified is empty or piece
      cannot move to new position

    - Raises [Invalid_piece] if piece does not belong to [plr] *)
