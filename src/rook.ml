(**Get all the steps between the origin and the destination. *)
let rec squares_between x1 y1 x2 y2 =
  if abs (x1 - x2) > 1 && y1 = y2 then
    let xstep = (x2 - x1) / abs (x2 - x1) in
    [ [ x1 + xstep; y1 ] ] @ squares_between (x1 + xstep) y1 x2 y2
  else if abs (y1 - y2) > 1 && x1 = x2 then
    let ystep = (y2 - y1) / abs (y2 - y1) in
    [ [ x1; y1 + ystep ] ] @ squares_between x1 (y1 + ystep) x2 y2
  else []

(** Had to make an alternative to List.for_all which returns false when the list
    is empty*)
let alt_for_all f lst =
  match lst with
  | [] -> true
  | _ -> List.for_all f lst

let node1 brd lst =
  let nth = List.nth lst in

  let is_space_empty lst =
    let x = List.nth lst 0 in
    let y = List.nth lst 1 in
    match Board.get_square brd x y with
    | Board.Empty -> true
    | Board.Piece p -> false
  in

  let steps = squares_between (nth 1) (nth 0) (nth 3) (nth 2) in
  alt_for_all is_space_empty steps

let node2 lst =
  let nth = List.nth lst in

  (nth 1 = nth 3 && nth 0 <> nth 2) (*Horizontal*)
  || (nth 0 = nth 2 && nth 1 <> nth 3)
(*Vertical*)
(* =============================================================== *)

let validate_rook_move (brd : Board.board) (plr : Player.player)
    (lst : int list) =
  node2 lst && node1 brd lst
