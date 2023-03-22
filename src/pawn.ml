(**)

type move =
  | Normal of Board.board
  | Final_row of Board.board
  | Illegal
(*Add a Capture?*)

let actual_move brd lst =
  let piece = Board.get_square brd (List.nth lst 1) (List.nth lst 0) in
  let board = Board.set_square (List.nth lst 3) (List.nth lst 2) brd piece in
  Board.set_square (List.nth lst 1) (List.nth lst 0) board Board.Empty

(**wd1, bd2 has the final row been reached?*)
let wd1 idx = idx = 0

let bd1 idx = idx = 7

(** d2 is there a piece at target destination?*)
let d2 (brd : Board.board) x y =
  let dest = Board.get_square brd x y in
  match dest with
  | Board.Empty -> false
  | Board.Piece p -> true

(** Is the pawn been moved soley vertically?*)
let d3 x1 y1 x2 y2 = y1 = y2 && (abs (x1 - x2) = 1 || abs (x1 - x2) = 2)

(** Is the target destination a valid position on the board?*)
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

(** sr_wd4, d4, sr_bd4, bd4 is the target a valid destination? *)
let start_row_wd4 (lst : int list) =
  let nth = List.nth lst in
  onBoard lst
  && begin
       (nth 0 = nth 2 && nth 1 - nth 3 = 1) (*Vertical*)
       || (nth 0 == nth 2 && nth 1 - nth 3 = 2) (*Two steps up*)
       || (nth 1 - nth 3 = 1 && nth 0 - nth 2 = 1) (*Upper-left*)
       || (nth 1 - nth 3 = 1 && nth 2 - nth 0 = 1) (*Upper-right*)
     end

let wd4 (lst : int list) =
  let nth = List.nth lst in
  onBoard lst
  && begin
       (nth 0 = nth 2 && nth 1 - nth 3 = 1) (*Vertical*)
       || (nth 1 - nth 3 = 1 && nth 0 - nth 2 = 1) (*Upper-left*)
       || (nth 1 - nth 3 = 1 && nth 2 - nth 0 = 1) (*Upper-right*)
     end

let start_row_bd4 (lst : int list) =
  let nth = List.nth lst in
  onBoard lst
  && begin
       (nth 0 = nth 2 && nth 3 - nth 1 = 1) (*Vertical*)
       || (nth 0 = nth 2 && nth 3 - nth 1 = 2) (*Two steps up*)
       || (nth 1 - nth 3 = 1 && nth 2 - nth 0 = 1) (*Lower-left*)
       || (nth 3 - nth 1 = 1 && nth 2 - nth 0 = 1) (*Lower-right*)
     end

let bd4 (lst : int list) =
  let nth = List.nth lst in
  onBoard lst
  && begin
       (nth 0 = nth 2 && nth 3 - nth 1 = 1) (*Vertical*)
       || (nth 3 - nth 1 = 1 && nth 0 - nth 2 = 1) (*Lower-left*)
       || (nth 3 - nth 1 = 1 && nth 2 - nth 0 = 1) (*Lower-right*)
     end

(**wd5, bd5 Is the pawn on the starting row?*)
let wd5 x = x = 6

let bd5 x = x = 1

(* ======================================================================= *)

(**Node 3 for the white decision subtree*)
let wd3sub brd lst =
  let nth = List.nth lst in
  if d3 (nth 1) (nth 0) (nth 3) (nth 2) then
    if d2 brd (nth 3) (nth 2) then Illegal
    else if wd1 (nth 3) then Final_row (actual_move brd lst)
    else Normal (actual_move brd lst)
  else if d2 brd (nth 3) (nth 2) then
    if wd1 (nth 3) then Final_row (actual_move brd lst)
    else Normal (actual_move brd lst)
  else Illegal

let bd3sub brd lst =
  let nth = List.nth lst in
  if d3 (nth 1) (nth 0) (nth 3) (nth 2) then
    if d2 brd (nth 3) (nth 2) then Illegal
    else if bd1 (nth 3) then Final_row (actual_move brd lst)
    else Normal (actual_move brd lst)
  else if d2 brd (nth 3) (nth 2) then Normal (actual_move brd lst)
  else Illegal

let white_move (brd : Board.board) (lst : int list) =
  let nth = List.nth lst in
  if wd5 (nth 1) then if start_row_wd4 lst then wd3sub brd lst else Illegal
  else if wd4 lst then wd3sub brd lst
  else Illegal

let black_move (brd : Board.board) (lst : int list) =
  let nth = List.nth lst in
  if bd5 (nth 1) then if start_row_bd4 lst then bd3sub brd lst else Illegal
  else if bd4 lst then bd3sub brd lst
  else Illegal

(* ============================================================== *)

let move_pawn (brd : Board.board) (plr : Player.player) (des : int list) =
  match plr with
  | Player.White -> white_move brd des
  | Player.Black -> black_move brd des
