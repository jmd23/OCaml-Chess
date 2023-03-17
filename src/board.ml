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

let rank_1 =
  [
    Piece (Rook "white");
    Piece (Knight "white");
    Piece (Bishop "white");
    Piece (Queen "white");
    Piece (King "white");
    Piece (Bishop "white");
    Piece (Knight "white");
    Piece (Rook "white");
  ]

let rank_2 =
  [
    Piece (Pawn "white");
    Piece (Pawn "white");
    Piece (Pawn "white");
    Piece (Pawn "white");
    Piece (Pawn "white");
    Piece (Pawn "white");
    Piece (Pawn "white");
    Piece (Pawn "white");
  ]

let rank_7 =
  [
    Piece (Pawn "black");
    Piece (Pawn "black");
    Piece (Pawn "black");
    Piece (Pawn "black");
    Piece (Pawn "black");
    Piece (Pawn "black");
    Piece (Pawn "black");
    Piece (Pawn "black");
  ]

let rank_8 =
  [
    Piece (Rook "black");
    Piece (Knight "black");
    Piece (Bishop "black");
    Piece (Queen "black");
    Piece (King "black");
    Piece (Bishop "black");
    Piece (Knight "black");
    Piece (Rook "black");
  ]

let empty_rank = [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ]

let piece_to_string = function
  | Pawn p -> if p = "white" then "♙" else "♟"
  | Bishop b -> if b = "white" then "♗" else "♝"
  | Knight n -> if n = "white" then "♘" else "♞"
  | Rook r -> if r = "white" then "♖" else "♜"
  | Queen q -> if q = "white" then "♕" else "♛"
  | King k -> if k = "white" then "♔" else "♚"

let print_square = function
  | Empty -> " "
  | Piece p -> piece_to_string p

let row_to_string row =
  "_________________\n"
  ^ List.fold_left
      (fun x y -> x ^ "|" ^ y)
      ""
      (List.map (fun p -> print_square p) row)
  ^ "|\n"

let starting_board =
  [
    rank_1;
    rank_2;
    empty_rank;
    empty_rank;
    empty_rank;
    empty_rank;
    rank_7;
    rank_8;
  ]

let board_to_string (b : board) =
  let new_b = List.rev b in
  List.fold_left
    (fun x y -> x ^ y)
    "\n"
    (List.map (fun x -> row_to_string x) new_b)

(*let full_board = board_to_string starting_board |> print_endline*)

let set_row j row value = List.mapi (fun a x -> if a = j then value else x) row

let set_square i j board value =
  List.mapi (fun a x -> if a = i then set_row j x value else x) board

let new_board = set_square 1 3 starting_board Empty
(*let test = board_to_string new_board |> print_endline*)
