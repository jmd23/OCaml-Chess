let is_empty (brd : Board.board) x y =
  let dest = Board.get_square brd x y in
  match dest with
  | Board.Empty -> true
  | Board.Piece p -> false

let radius_one (lst : int list) =
  abs (List.nth lst 1 - List.nth lst 3) <= 1
  && abs (List.nth lst 0 - List.nth lst 2) <= 1
  && (List.nth lst 1 != List.nth lst 3 || List.nth lst 0 != List.nth lst 2)

let can_castle (brd : Board.board) (lst : int list) plr =
  match (List.nth lst 0, List.nth lst 1) with
  | 4, 7 -> (
      match (List.nth lst 2, List.nth lst 3) with
      | 6, 7 ->
          is_empty brd 7 5 && is_empty brd 7 6
          && Board.get_square brd 7 7 = Piece (Rook plr)
      | 2, 7 ->
          is_empty brd 7 1 && is_empty brd 7 2 && is_empty brd 7 3
          && Board.get_square brd 7 0 = Piece (Rook plr)
      | _ -> false)
  | 4, 0 -> (
      match (List.nth lst 2, List.nth lst 3) with
      | 6, 0 ->
          is_empty brd 0 5 && is_empty brd 0 6
          && Board.get_square brd 0 7 = Piece (Rook plr)
      | 2, 0 ->
          is_empty brd 0 1 && is_empty brd 0 2 && is_empty brd 0 3
          && Board.get_square brd 0 0 = Piece (Rook plr)
      | _ -> false)
  | _ -> false

let validate_king_move (brd : Board.board) (plr : Player.player)
    (lst : int list) =
  radius_one lst || can_castle brd lst plr
