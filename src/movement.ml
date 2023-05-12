exception Invalid_move
exception Invalid_piece

type result =
  | Normal of Board.board
  | Captured of (Board.board * Board.piece)

(** Checks if the player moving the piece owns that piece. *)
let validate_owner (mover : Player.player) (p : Board.piece) =
  match p with
  | Pawn plr -> mover = plr
  | Knight plr -> mover = plr
  | King plr -> mover = plr
  | Queen plr -> mover = plr
  | Rook plr -> mover = plr
  | Bishop plr -> mover = plr

(** Checks the ownership of the piece on the destination square, if any, to make
    sure a piece doesn't capture another piece on the same side. *)
let validate_destination_piece (brd : Board.board) (ply : Player.player)
    (lst : int list) =
  let dest_square = Board.get_square brd (List.nth lst 3) (List.nth lst 2) in
  match dest_square with
  | Empty -> true
  | Piece p -> not (validate_owner ply p)

(** Checks if co-ordinates provided are on the board*)
let onBoard lst =
  let nth = List.nth lst in
  nth 0 >= 0
  && nth 0 <= 7
  && nth 1 >= 0
  && nth 1 <= 7
  && nth 2 >= 0
  && nth 2 <= 7
  && nth 3 >= 0
  && nth 3 <= 7

let handle_rook brd lst ply =
  match (List.nth lst 2, List.nth lst 3) with
  | 6, 7 ->
      let new_brd = Board.set_square 7 7 brd Board.Empty in
      Board.set_square 7 5 new_brd (Board.Piece (Rook ply))
  | 2, 7 ->
      let new_brd = Board.set_square 7 0 brd Board.Empty in
      Board.set_square 7 3 new_brd (Board.Piece (Rook ply))
  | 6, 0 ->
      let new_brd = Board.set_square 0 7 brd Board.Empty in
      Board.set_square 0 5 new_brd (Board.Piece (Rook ply))
  | 2, 0 ->
      let new_brd = Board.set_square 0 0 brd Board.Empty in
      Board.set_square 0 3 new_brd (Board.Piece (Rook ply))
  | _ -> brd

(** Performs the actual moving of the piece. Requires all appropriate checks to
    have passed. Returns a [Normal b] or [Captured]. *)
let move_piece brd lst ply =
  let origin_square = Board.get_square brd (List.nth lst 1) (List.nth lst 0) in
  let destination_square =
    Board.get_square brd (List.nth lst 3) (List.nth lst 2)
  in
  let board =
    Board.set_square (List.nth lst 3) (List.nth lst 2) brd origin_square
  in
  let res =
    if
      origin_square = Board.Piece (King ply)
      && abs (List.nth lst 2 - List.nth lst 0) = 2
    then handle_rook board lst ply
    else board
  in
  match destination_square with
  | Board.Empty ->
      Normal
        (Board.set_square (List.nth lst 1) (List.nth lst 0) res Board.Empty)
  | Piece p ->
      let nbrd =
        Board.set_square (List.nth lst 1) (List.nth lst 0) res Board.Empty
      in
      Captured (nbrd, p)

(** Validates the move of a piece using a given . Returns the result of moving
    piece*)
let validate (brd : Board.board) (ply : Player.player) (lst : int list)
    validator =
  if validator brd ply lst then move_piece brd lst ply else raise Invalid_move

(** Move validator for Queen. Depends on validators of both Bishop and Rook*)
let validate_queen_move (brd : Board.board) (ply : Player.player)
    (lst : int list) =
  Bishop.validate_bishop_move brd ply lst || Rook.validate_rook_move brd ply lst

(** Ensures destination can be moved to and then matches each piece with it's
    appropriate decision tree*)
let handle_piece (brd : Board.board) (ply : Player.player) (piece : Board.piece)
    (lst : int list) =
  if not (validate_destination_piece brd ply lst) then raise Invalid_move
  else
    let partial_val = validate brd ply lst in
    match piece with
    | Pawn plr -> partial_val Pawn.validate_pawn_move
    | Bishop plr -> partial_val Bishop.validate_bishop_move
    | Knight plr -> partial_val Knight.validate_knight_move
    | Rook plr -> partial_val Rook.validate_rook_move
    | Queen plr -> partial_val validate_queen_move
    | King plr -> partial_val King.validate_king_move

let move (brd : Board.board) (ply : Player.player) (lst : int list) =
  (*Check if the co-ordinates are valid.*)
  if onBoard lst then
    (*Get the origin and do some pattern matching*)
    let origin_square =
      Board.get_square brd (List.nth lst 1) (List.nth lst 0)
    in
    match origin_square with
    | Empty -> raise Invalid_move
    | Piece p ->
        if validate_owner ply p then handle_piece brd ply p lst
        else raise Invalid_piece
  else raise Invalid_move
