let radius_one (lst : int list) =
  abs (List.nth lst 1 - List.nth lst 3) <= 1
  && abs (List.nth lst 0 - List.nth lst 2) <= 1
  && (List.nth lst 1 != List.nth lst 3 || List.nth lst 0 != List.nth lst 2)

let validate_king_move (brd : Board.board) (plr : Player.player)
    (lst : int list) =
  radius_one lst
