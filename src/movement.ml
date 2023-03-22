
exception Invalid_move
exception Invalid_piece

let validate_owner (mover: Player.player) (p:Board.piece) = 
  match p with 
  | Pawn plr -> mover = plr
  | Knight plr -> mover = plr
  | King plr -> mover = plr
  | Queen plr -> mover = plr
  | Rook plr -> mover = plr
  | Bishop plr -> mover = plr



(** Assuming the player moving it owns the pawn.*)
let move_pawn (brd: Board.board) (plr : Player.player) (lst : int list)=
  let result = Pawn.move_pawn brd plr lst in
  match result with 
  | Pawn.Normal r -> r
  | Pawn.Final_row r -> r
  | Illegal -> raise Invalid_move

let move_piece (brd : Board.board) (ply : Player.player) (lst : int list) : Board.board =
  let og_square = Board.get_square brd (List.nth lst 1) (List.nth lst 0) in
  match og_square with 
  | Empty -> raise Invalid_move
  | Piece p ->
    if validate_owner ply p then
    begin
      match p with 
      | Pawn plr -> move_pawn brd plr lst
      | Bishop plr -> raise Invalid_move
      | Knight plr -> raise Invalid_move
      | Rook plr -> raise Invalid_move
      | Queen plr -> raise Invalid_move
      | King plr -> raise Invalid_move
    end
  else raise Invalid_piece
