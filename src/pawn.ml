(**)

(** d2 is there a piece at target destination?*)
let node2 (brd : Board.board) x y =
  let dest = Board.get_square brd x y in
  match dest with
  | Board.Empty -> false
  | Board.Piece p -> true

(** Is the pawn been moved soley vertically?*)
let node3 x1 y1 x2 y2 = y1 = y2 && (abs (x1 - x2) = 1 || abs (x1 - x2) = 2)

(** sr_wd4, d4, sr_bd4, bd4 is the target a valid destination? *)
let start_row_wd4 (lst : int list) (brd : Board.board) =
  let nth = List.nth lst in
  begin
    (nth 0 = nth 2 && nth 1 - nth 3 = 1) (*Vertical*)
    || nth 0 == nth 2
       && nth 1 - nth 3 = 2
       && not (node2 brd (nth 3 + 1) (nth 2))
       (*Two steps up*)
    || (nth 1 - nth 3 = 1 && nth 0 - nth 2 = 1) (*Upper-left*)
    || (nth 1 - nth 3 = 1 && nth 2 - nth 0 = 1) (*Upper-right*)
  end

let node4_white (lst : int list) =
  let nth = List.nth lst in
  begin
    (nth 0 = nth 2 && nth 1 - nth 3 = 1) (*Vertical*)
    || (nth 1 - nth 3 = 1 && nth 0 - nth 2 = 1) (*Upper-left*)
    || (nth 1 - nth 3 = 1 && nth 2 - nth 0 = 1) (*Upper-right*)
  end

let start_row_bd4 (lst : int list) (brd : Board.board) =
  let nth = List.nth lst in
  begin
    (nth 0 = nth 2 && nth 3 - nth 1 = 1) (*Vertical*)
    || nth 0 = nth 2
       && nth 3 - nth 1 = 2
       && not (node2 brd (nth 1 + 1) (nth 0))
       (*Two steps down*)
    || (nth 3 - nth 1 = 1 && nth 0 - nth 2 = 1) (*Lower-left*)
    || (nth 3 - nth 1 = 1 && nth 2 - nth 0 = 1) (*Lower-right*)
  end

let node4_black (lst : int list) =
  let nth = List.nth lst in
  begin
    (nth 0 = nth 2 && nth 3 - nth 1 = 1) (*Vertical*)
    || (nth 3 - nth 1 = 1 && nth 0 - nth 2 = 1) (*Lower-left*)
    || (nth 3 - nth 1 = 1 && nth 2 - nth 0 = 1) (*Lower-right*)
  end

(**wd5, bd5 Is the pawn on the starting row?*)
let node5_white x = x = 6

let node5_black x = x = 1

(* ======================================================================= *)

(**Node 3 for the white decision subtree*)
let node3_subtree brd lst =
  let nth = List.nth lst in
  if node3 (nth 1) (nth 0) (nth 3) (nth 2) then not (node2 brd (nth 3) (nth 2))
  else node2 brd (nth 3) (nth 2)

let white_move (brd : Board.board) (lst : int list) =
  let nth = List.nth lst in
  if node5_white (nth 1) then start_row_wd4 lst brd && node3_subtree brd lst
  else node4_white lst && node3_subtree brd lst

let black_move (brd : Board.board) (lst : int list) =
  let nth = List.nth lst in
  if node5_black (nth 1) then start_row_bd4 lst brd && node3_subtree brd lst
  else node4_black lst && node3_subtree brd lst

(* ============================================================== *)

let validate_pawn_move (brd : Board.board) (plr : Player.player)
    (des : int list) =
  match plr with
  | Player.White -> white_move brd des
  | Player.Black -> black_move brd des
