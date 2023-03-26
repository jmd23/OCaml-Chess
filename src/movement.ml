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

(** Performs the actual moving of the piece. Requires all appropriate checks to
    have passed. Returns a [Normal b] or [Captured]. *)
let move_piece brd lst =
  let origin_square = Board.get_square brd (List.nth lst 1) (List.nth lst 0) in
  let destination_square =
    Board.get_square brd (List.nth lst 3) (List.nth lst 2)
  in
  let board =
    Board.set_square (List.nth lst 3) (List.nth lst 2) brd origin_square
  in

  match destination_square with
  | Board.Empty ->
      Normal
        (Board.set_square (List.nth lst 1) (List.nth lst 0) board Board.Empty)
  | Piece p ->
      let nbrd =
        Board.set_square (List.nth lst 1) (List.nth lst 0) board Board.Empty
      in
      Captured (nbrd, p)

let move_pawn (brd : Board.board) (plr : Player.player) (lst : int list) =
  if Pawn.validate_pawn_move brd plr lst then move_piece brd lst
  else raise Invalid_move

let move_bishop (brd : Board.board) (plr : Player.player) (lst : int list) =
  if Bishop.validate_bishop_move brd plr lst then move_piece brd lst
  else raise Invalid_move

(** Matches each piece with it's appropriate decision tree*)
let handle_piece (brd : Board.board) (ply : Player.player) (piece : Board.piece)
    (lst : int list) =
  match piece with
  | Pawn plr -> move_pawn brd plr lst
  | Bishop plr -> move_bishop brd plr lst
  | Knight plr -> raise Invalid_move
  | Rook plr -> raise Invalid_move
  | Queen plr -> raise Invalid_move
  | King plr -> raise Invalid_move

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
        if validate_owner ply p then
          if validate_destination_piece brd ply lst then
            handle_piece brd ply p lst
          else raise Invalid_move
        else raise Invalid_piece
  else raise Invalid_move
