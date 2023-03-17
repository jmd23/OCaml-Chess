type piece =
  | Pawn of string
  | Bishop of string
  | Knight of string
  | Rook of string
  | Queen of string
  | King of string

type square =
  | Piece of piece
  | Empty

type board = square list list

val rank_1 : square list
val rank_2 : square list
val rank_7 : square list
val rank_8 : square list
val empty_rank : square list
val piece_to_string : piece -> string
val print_square : square -> string
val row_to_string : square list -> string
val starting_board : square list list
val board_to_string : board -> string
val set_row : int -> 'a list -> 'a -> 'a list
val set_square : int -> int -> 'a list list -> 'a -> 'a list list
val new_board : square list list