(** The type representing a piece on the board, contains information about which
    player possesses that piece*)
type piece =
  | Pawn of Player.player
  | Bishop of Player.player
  | Knight of Player.player
  | Rook of Player.player
  | Queen of Player.player
  | King of Player.player

(** The type representing a square on the board, either a Piece or Empty*)
type square =
  | Piece of piece
  | Empty

(* The type representing a square on the board, either a Piece or Empty*)

type board = square list list
(** The type representing a chess board, a 2d list of squares*)

val piece_to_string : piece -> string
(** [piece_to_string p] returns the string representation, in unicode, of piece
    p, with the proper color.*)

val print_square : square -> string
(** [print_square s] returns the string representation of a square, which can be
    a piece or Empty.*)

val row_to_string : int -> square list -> string
(** [row_to_string i s] returns the string representation of row i on the
    chessboard, s, which is a list of squares.*)

val starting_board : square list list
(** starting_board is data that represents the original board*)

val board_to_string : board -> string
(** [board_to_string b] returns the string representation of the entire board,
    including the rank and file labels*)

val set_row : int -> 'a list -> 'a -> 'a list
(** [set_row i row p] sets the square at index i of square list row to p, and
    returns the new row. Requires: 0 <= i < length row. This is used primarily
    as a helper to set_square*)

val set_square : int -> int -> 'a list list -> 'a -> 'a list list
(** [set_square i j board p] sets the square at index i of square list row to p,
    and returns the new board. Requires: [i, j] is a valid index of board.*)

val get_square : 'a list list -> int -> int -> 'a
(** [get_square board i j] gets the value of the board at row i, index j.
    Requires: [i, j] is a valid index of board.*)

val print_board : board -> unit
(** [print_board brd] prints brd to the standard output with visually distinct
    colors for each player's pieces*)

val validate_owner : Player.player -> piece -> bool
(** [validate_owner plr p] returns a boolean, for whether the piece object p
    belongs to the player plr *)

val board_equal : board -> board -> bool
(** [board_equal b1 b2] returns a boolean as to whether two boards are equal.
    Two boards are equal if they have the same piece in every square. *)
