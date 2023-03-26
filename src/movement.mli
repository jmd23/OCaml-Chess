(** The result of successfully moving a piece on a board.

    - [Normal brd] for a normal successful move where no piece is captured
    - [Captured (brd, piece)] for when a piece is captured by the moving piece *)
type result =
  | Normal of Board.board
  | Captured of (Board.board * Board.piece)

exception Invalid_move
exception Invalid_piece

val move : Board.board -> Player.player -> int list -> result
(** [move_piece brd plr lst] moves the player's,[plr], piece at lst[1],lst[0] on
    board, [brd]. Returns [Normal] or [Captured] if successful. Does not check
    if a pawn has reached the final row.

    - Raises [Invalid_move] if origin position specified is empty or piece
      cannot move to new position

    - Raises [Invalid_piece] if piece does not belong to [plr] *)
